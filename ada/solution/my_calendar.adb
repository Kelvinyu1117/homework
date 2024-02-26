with Ada.Strings.Unbounded, Ada.Text_IO, Ada.Integer_Text_IO;
with Ada.Text_IO.Unbounded_IO; 
with Ada.Containers.Indefinite_Vectors; 
use  Ada.Strings.Unbounded, Ada.Text_IO, Ada.Containers;

procedure My_Calendar is
    package Integer_Vector is new Indefinite_Vectors(Natural, Integer);
    type Month_Text_Array is array (1 .. 12) of Unbounded_String;
    type Week_Text_Array is array (1 .. 7) of String(1..2);
    type Month_Day_Array is array(1..7, 0..12 * 7 - 1) of Integer;

    n_col: constant Integer := 3;

    n_days_in_month_table: constant array(1 .. 12) of Integer := (
        31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31);

    month_eng: constant Month_Text_Array := (
      To_Unbounded_String("January"),
      To_Unbounded_String("February"),
      To_Unbounded_String("March"),
      To_Unbounded_String("April"),
      To_Unbounded_String("May"),
      To_Unbounded_String("June"),
      To_Unbounded_String("July"),
      To_Unbounded_String("August"),
      To_Unbounded_String("September"),
      To_Unbounded_String("October"),
      To_Unbounded_String("November"),
      To_Unbounded_String("December"));

    month_fr: constant Month_Text_Array := (
      To_Unbounded_String("janvier"),
      To_Unbounded_String("février"),
      To_Unbounded_String("mars"),
      To_Unbounded_String("avril"),
      To_Unbounded_String("mai"),
      To_Unbounded_String("juin"),
      To_Unbounded_String("juillet"),
      To_Unbounded_String("aout"),
      To_Unbounded_String("septembre"),
      To_Unbounded_String("octobre"),
      To_Unbounded_String("novembre"),
      To_Unbounded_String("décembre"));
    
    week_eng: constant Week_Text_Array := (
      "Su",
      "Mo",
      "Tu",
      "We",
      "Th",
      "Fr",
      "Sa");

    week_fr: constant Week_Text_Array := (
      "Di",
      "Lu",
      "Ma",
      "Me",
      "Je",
      "Ve",
      "Sa");

    function Month_Text(lang: in Unbounded_String) return Month_Text_Array is
    begin
      if lang = "English" then
        return month_eng;
      else
        return month_fr;
      end if;
    end Month_Text;

    function Week_Text(lang: in Unbounded_String) return Week_Text_Array is  
    begin
      if lang = "English" then
        return week_eng;
      else
        return week_fr;
      end if;
    end Week_Text;

    function Is_Valid(year: in Integer) return Boolean is
    begin
      return year >= 1582;
    end Is_Valid;

    function Leap_Year(year: in Integer) return Boolean is
    begin
      if (year mod 4 /= 0) or (year mod 100 = 0 and year mod 400 /= 0) then
        return False;
      else
        return True;
      end if;
    end Leap_Year;

    function Num_Days_in_Month(month: in Integer; year: in Integer) return Integer is
    begin
      if Leap_Year(year) and month = 2 then
        return n_days_in_month_table(month) + 1;
      else
        return n_days_in_month_table(month);
      end if;
    end Num_Days_in_Month;
  
    procedure Build_Calendar(year: in Integer; first_day_of_jan: in Integer; month_days: in out Month_Day_Array) is
      month: Integer := 1;
      week: Integer;
      j: Integer;
    begin
      -- handle leading blanks
      for i in 1..(first_day_of_jan - 1) loop
        month_days(1, i) := 0;
      end loop;

      j := first_day_of_jan;

      while month <= 12 loop
        week := 1;
        
        for day in 1..Num_Days_in_Month(month, year) loop
          month_days(week, (month - 1) * 7 + j) := day;  
          j := (j + 1) mod 7;
          if j = 0 then
            week := week + 1;
          end if;
        end loop;

        for i in ((month - 1) * 7 + j)..(month * 7 - 1) loop
          month_days(week, i) := 0;
        end loop;

        month := month + 1;

      end loop;
    end Build_Calendar;
    
    procedure Print_Padding(padding: in Integer) is
    begin
      for i in 1..padding loop
          Ada.Text_IO.Put(" ");
        end loop;
    end Print_Padding;

    procedure Print_Row_Heading(month_text: in Month_Text_Array; week_text: in Week_Text_Array; row: in Integer) is
      s: constant Integer := (row - 1) * n_col + 1;
      total_width : constant Integer := 20;
  
      procedure Print_With_Padding(s: Unbounded_String) is  
        left_padding:  constant Integer := (total_width - Ada.Strings.Unbounded.Length(s)) / 2;
        right_padding: constant Integer := total_width - Ada.Strings.Unbounded.Length(s) - left_padding;
      begin
        Print_Padding(left_padding);
        Ada.Text_IO.Put(Ada.Strings.Unbounded.To_String(s));
        Print_Padding(right_padding);

      end Print_With_Padding;
    
    begin
      for i in s .. (s + 2) loop
        Print_With_Padding(month_text(i));
        Print_Padding(6);
      end loop;
    
      Ada.Text_IO.New_Line;
      for i in s .. (s + 2) loop
        for e of week_text loop
          Ada.Text_IO.Put(e);
          Ada.Text_IO.Put(" ");
        end loop;
        Print_Padding(5);
      end loop;
      Ada.Text_IO.New_Line;
    
    end Print_Row_Heading;

    procedure Banner(year: in Integer; indent: in Integer) is
      type Number_Banners is array (0 .. 9, 0 .. 9) of String(1 .. 10);
      num_banners: constant Number_Banners :=
        (   -- Number 0
            (
                "  000000  ",
                " 0      0 ",
                " 0      0 ",
                " 0      0 ",
                " 0      0 ",
                " 0      0 ",
                " 0      0 ",
                " 0      0 ",
                " 0      0 ",
                "  000000  "),
                -- Number 1
            (
                "     1    ",
                "    11    ",
                "  1  1    ",
                "     1    ",
                "     1    ",
                "     1    ",
                "     1    ",
                "     1    ",
                "     1    ",
                " 11111111 "),
            -- Number 2
            (
                "  222222  ",
                " 2     2  ",
                "       2  ",
                "       2  ",
                "       2  ",
                "  222222  ",
                " 2        ",
                " 2        ",
                " 2        ",
                " 22222222 "),

            -- Number 3
            (
                "  333333  ",
                " 3      3 ",
                "        3 ",
                "        3 ",
                "        3 ",
                "  333333  ",
                "        3 ",
                "        3 ",
                " 3      3 ",
                "  3333333 "),

            -- Number 4
            (
                " 4        ",
                " 4    4   ",
                " 4    4   ",
                " 4    4   ",
                " 4    4   ",
                " 44444444 ",
                "     4    ",
                "     4    ",
                "     4    ",
                "     4    "),

            -- Number 5
            (
                " 55555555 ",
                " 5        ",
                " 5        ",
                " 5        ",
                " 5555555  ",
                "        5 ",
                "        5 ",
                "        5 ",
                " 5      5 ",
                "  555555  "),

            -- Number 6
            (
                "  666666  ",
                " 6      6 ",
                " 6        ",
                " 6        ",
                " 66666666 ",
                " 6      6 ",
                " 6      6 ",
                " 6      6 ",
                "  6    6  ",
                "   6666   "),

            -- Number 7
            (
                " 77777777 ",
                " 7     7  ",
                "      7   ",
                "     7    ",
                "    7     ",
                "   7      ",
                "   7      ",
                "   7      ",
                "   7      ",
                "   7      " ),
            -- Number 8
            (
                " 8888888  ",
                " 8     8  ",
                " 8     8  ",
                " 8     8  ",
                " 8     8  ",
                "  88888   ",
                " 8     8  ",
                " 8     8  ",
                " 8     8  ",
                " 8888888  "),
            -- Number 9
            (
                " 9999999  ",
                " 9     9  ",
                " 9     9  ",
                " 9     9  ",
                " 9     9  ",
                "  999999  ",
                "       9  ",
                " 9     9  ",
                " 9     9  ",
                "  999999  "));

      year_digits: Integer_Vector.Vector;
      y: Integer := year;
      d: Integer;
    begin
      while y /= 0 loop
          d := y mod 10;
          year_digits.Append(Natural(d));
          y := y / 10;
      end loop;

      Ada.Text_IO.New_Line;
      Ada.Text_IO.New_Line;

      for i in 0..9 loop
        for e of reverse year_digits loop
            for j in 1 .. indent loop
              Ada.Text_IO.Put(" ");
            end loop;
            Ada.Text_IO.Put(num_banners(e, i));
        end loop;
        Print_Padding(10);
        Ada.Text_IO.New_Line;
      end loop;

      Ada.Text_IO.New_Line;
      Ada.Text_IO.New_Line;
    end Banner;

    procedure Read_Cal_Info(year: out Integer; first_day: out Integer; lang: out Unbounded_String) is
      y: Integer;
      ll: Unbounded_String;
    begin
      loop
        Put("Enter year: ");
        Ada.Integer_Text_IO.Get(y);
        if Is_Valid(y) then
            exit;
        end if;
        Put_Line("Year is not valid, please input again!");
      end loop;
      
      Skip_Line;

      loop
        Put("Enter language (English/French): ");
        Ada.Text_IO.Unbounded_IO.Get_Line(ll);
        if ll = To_Unbounded_String("English") or ll = To_Unbounded_String("French") then
            exit;
        end if;
        Put_Line("language is not valid, please input again!");
      end loop;

      year := y;
      lang := ll;

      y := year - 1;
      first_day :=  (36 + y + (y / 4) - (y / 100) + (y / 400)) mod 7;
    end Read_Cal_Info;

    month_days: Month_Day_Array := (others => (others => 0));
    year: Integer;
    first_day: Integer;
    ident: constant Integer := 6;
    lang: Unbounded_String;
    cols: Integer := 1;
begin
    Read_Cal_Info(year, first_day, lang);
    
    Banner(year, ident);
    Build_Calendar(year, first_day, month_days);
    
    for heading_row in 1..4 loop
      Print_Row_Heading(Month_Text(lang), Week_Text(lang), heading_row);
      for i in 1..7 loop
        for j in (cols - 1) * 7 .. (cols + 2) * 7 - 1 loop
          if month_days(i, j) = 0 then
            Print_Padding(2);
          else
            Ada.Integer_Text_IO.Put(month_days(i, j), Width => 2);
          end if;
          
          Print_Padding(1);
          if j /= 0 and (j + 1) mod 7 = 0 then
            Print_Padding(5);
          end if;
        end loop;
        Ada.Text_IO.New_Line;
      end loop;
      cols := cols + 3;
    end loop;
end My_Calendar;
