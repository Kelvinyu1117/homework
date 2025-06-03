#include <cmath>
#include <iostream>
#include <pybind11/numpy.h>
#include <pybind11/pybind11.h>

namespace py = pybind11;

class matrix_view {
private:
  float *data;
  size_t rows;
  size_t cols;

public:
  matrix_view(const float *data, size_t rows, size_t cols)
      : data(const_cast<float *>(data)), rows(rows), cols(cols) {}

  float &operator()(size_t i, size_t j) { return data[i * cols + j]; }

  const float &operator()(size_t i, size_t j) const {
    return data[i * cols + j];
  }

  void multiply(const matrix_view other, matrix_view result) const {
    if (cols != other.rows) {
      throw std::invalid_argument(
          "matrix_view dimensions do not match for multiplication");
    }

    for (size_t i = 0; i < rows; ++i) {
      for (size_t j = 0; j < other.cols; j++) {
        result(i, j) = 0;
        for (size_t k = 0; k < cols; ++k) {
          result(i, j) += (*this)(i, k) * other(k, j);
        }
      }
    }
  }

  matrix_view select_rows(size_t start, size_t end) const {
    if (start >= rows || end > rows || start >= end) {
      throw std::out_of_range("Invalid row selection range");
    }
    return matrix_view(data + start * cols, end - start, cols);
  }

  matrix_view &operator-=(const matrix_view &other) {
    if (rows != other.rows || cols != other.cols) {
      throw std::invalid_argument(
          "matrix_view dimensions do not match for subtraction");
    }

    for (size_t i = 0; i < rows; ++i) {
      for (size_t j = 0; j < cols; j++) {
        (*this)(i, j) -= other(i, j);
      }
    }
    return *this;
  }

  matrix_view &operator*=(float scalar) {
    for (size_t i = 0; i < rows; ++i) {
      for (size_t j = 0; j < cols; j++) {
        (*this)(i, j) *= scalar;
      }
    }
    return *this;
  }

  template <typename T> void apply_element_wise(T &&func) {
    for (size_t i = 0; i < rows; ++i) {
      for (size_t j = 0; j < cols; j++) {
        func((*this)(i, j));
      }
    }
  }

  template <typename T> void apply_rows(T &&func) {
    for (size_t i = 0; i < rows; ++i) {
      func(data + i * cols, cols);
    }
  }

  size_t get_rows() const { return rows; }

  size_t get_cols() const { return cols; }

  void print() const {
    for (size_t i = 0; i < rows; ++i) {
      for (size_t j = 0; j < cols; j++) {
        std::cout << (*this)(i, j) << " ";
      }
      std::cout << std::endl;
    }
  }
};

void eye_and_select_by(size_t k, const unsigned char *indexes, size_t start,
                       size_t end, matrix_view out) {

  for (size_t i = start; i < end; i++) {
    int label = indexes[i];
    for (size_t c = 0; c < k; c++) {
      out(i, c) = (c == static_cast<size_t>(label)) ? 1.0f : 0.0f;
    }
  }
}

void mat_mul_and_softmax(const matrix_view x, const matrix_view y,
                         matrix_view out) {
  // Compute logits
  x.multiply(y, out);
  out.apply_element_wise([](float &val) { val = std::exp(val); });

  out.apply_rows([](float *row, size_t columns) {
    float sum = 0.0f;
    for (size_t j = 0; j < columns; j++) {
      sum += row[j];
    }

    for (size_t j = 0; j < columns; j++) {
      row[j] /= sum;
    }
  });
}

void mat_transpose_mul(const matrix_view x, const matrix_view y,
                       matrix_view out) {
  for (size_t i = 0; i < x.get_cols(); ++i) {
    for (size_t j = 0; j < y.get_cols(); j++) {
      out(i, j) = 0;
      for (size_t k = 0; k < x.get_rows(); ++k) {
        out(i, j) += x(k, i) * y(k, j);
      }
    }
  }
}

void softmax_regression_epoch_cpp(const float *X, const unsigned char *y,
                                  float *theta, size_t m, size_t n, size_t k,
                                  float lr, size_t batch) {
  /**
   * A C++ version of the softmax regression epoch code.  This should run a
   * single epoch over the data defined by X and y (and sizes m,n,k), and
   * modify theta in place.  Your function will probably want to allocate
   * (and then delete) some helper arrays to store the logits and gradients.
   *
   * Args:
   *     X (const float *): pointer to X data, of size m*n, stored in row
   *          major (C) format
   *     y (const unsigned char *): pointer to y data, of size m
   *     theta (float *): pointer to theta data, of size n*k, stored in row
   *          major (C) format
   *     m (size_t): number of examples
   *     n (size_t): input dimension
   *     k (size_t): number of classes
   *     lr (float): learning rate / SGD step size
   *     batch (int): SGD minibatch size
   *
   * Returns:
   *     (None)
   */

  matrix_view x(X, m, n);
  matrix_view theta_v(theta, n, k);

  for (auto i = 0U; i < m; i += batch) {
    int j = std::min(i + batch, m);
    auto n_sample = j - i;
    const matrix_view X_sample = x.select_rows(i, j);
    auto M = X_sample.get_rows();

    float *I_y = new float[n_sample * k];
    matrix_view i_y(I_y, n_sample, k);
    eye_and_select_by(k, y + i, 0, n_sample, i_y);

    float *Z = new float[n_sample * k];
    matrix_view z(Z, n_sample, k);
    mat_mul_and_softmax(X_sample, theta_v, z);
    z -= i_y;

    float *grad = new float[n * k];
    matrix_view g(grad, n, k);
    mat_transpose_mul(X_sample, z, g);
    g *= (lr / M);
    theta_v -= g;

    delete[] I_y;
    delete[] Z;
    delete[] grad;
  }
  /// BEGIN YOUR CODE
  /// END YOUR CODE
}

/**
 * This is the pybind11 code that wraps the function above.  It's only role is
 * wrap the function above in a Python module, and you do not need to make any
 * edits to the code
 */
PYBIND11_MODULE(simple_ml_ext, m) {
  m.def(
      "softmax_regression_epoch_cpp",
      [](py::array_t<float, py::array::c_style> X,
         py::array_t<unsigned char, py::array::c_style> y,
         py::array_t<float, py::array::c_style> theta, float lr, int batch) {
        softmax_regression_epoch_cpp(
            static_cast<const float *>(X.request().ptr),
            static_cast<const unsigned char *>(y.request().ptr),
            static_cast<float *>(theta.request().ptr), X.request().shape[0],
            X.request().shape[1], theta.request().shape[1], lr, batch);
      },
      py::arg("X"), py::arg("y"), py::arg("theta"), py::arg("lr"),
      py::arg("batch"));
}
