(* check if a given date A (Y-M-D) (int*int*int) is older than date B *)

fun is_older (date1: int*int*int, date2: int*int*int) =
  let
    fun year (date: (int*int*int)) = #1 date
    fun month (date: (int*int*int)) = #2 date
    fun day (date: (int*int*int)) = #3 date
  in
    if  year(date1) < year(date2) then true
    else if year(date1) > year(date2) then false
    else
      if month(date1) < month(date2) then true
      else if month(date1) > month(date2) then false
      else
        if day(date1) < day(date1) then true
        else false
  end

(* ====================== *)
(* check how many dates are in the given month *)

fun number_in_month (dates: (int*int*int) list, month: int) =
  let
    fun check_date_in_month (date_month: (int*int*int), month) =
      if (#2 date_month) = month then 1 else 0

    fun check_dates (dates) =
      let
        val first_result = check_date_in_month ((hd dates), month)
      in
        if null (tl dates) then first_result
        else first_result + check_dates(tl dates)
      end
  in
    if null dates then 0 else check_dates (dates)
  end

(* ====================== *)
(* Check how many dates are in any of the given months *)

fun number_in_months (dates: (int*int*int) list, months: int list) =
  if null months then 0
  else
    let
      val base_count = number_in_month (dates, hd months)
    in
      if null (tl months) then base_count
      else base_count + number_in_months (dates, tl months)
    end

(* ====================== *)
(* Return the dates that are in the given month *)

fun dates_in_month (dates: (int*int*int) list, month: int) =
  let
    fun date_month_match (date: (int*int*int), month) =
      if (#2 date) = month then [date] else []

    fun check_dates (dates) =
      let
        val base_list = date_month_match (hd dates, month)
      in
        if null (tl dates) then base_list
        else
          if null base_list then check_dates(tl dates)
          else (hd base_list)::check_dates(tl dates)
      end
  in
    if null dates then [] else check_dates (dates)
  end

(* ====================== *)
(* Return dates that are in any of the given months *)

fun dates_in_months (dates: (int*int*int) list, months: int list) =
  if null months then []
  else
    let
      val base_list = dates_in_month (dates, hd months)
    in
      if null (tl months) then base_list
      else base_list @ dates_in_months (dates, tl months)
    end

(* ====================== *)
(* Return the nth element of a list *)

fun get_nth (words: string list, position: int) =
  let
    val counter = position - 1
  in
    if counter = 0 then (hd words)
    else get_nth (tl words, counter)
  end

(* ====================== *)
(* Convert date ints to string *)

fun date_to_string (year: int, month: int, day: int) =
  let
    val month_list = [ "January", "February", "March", "April", "May",
                       "June", "July", "August", "September", "October",
                       "November", "December" ]
    val month_name = get_nth (month_list, month)
    val day_string = Int.toString(day)
    val year_string = Int.toString(year)
  in
    month_name ^ " " ^ day_string ^ ", " ^ year_string
  end

(* ====================== *)
(* Return an int n such that the sum of n elements of list is less than sum  *)

fun number_before_reaching_sum (sum: int, items: int list) =
  let
    val counter = 1
    val first_check = sum - (hd items)
  in
    if first_check <= 0 orelse null (tl items)
    then 0
    else counter + number_before_reaching_sum (first_check, tl items)
  end

(* ====================== *)
(* Check what month the given day of the year is in *)

fun what_month (day: int) =
  let
    val months_days_count = [31, 28, 31, 30, 31, 30,
                             31, 30, 31, 30, 31, 30]
  in
    number_before_reaching_sum (day, months_days_count) + 1
  end

(* ====================== *)
(* Return a list containing the months of each day in a given range *)

fun month_range (day1: int, day2: int) =
  if day1 > day2 then []
  else
    let
      val month_list = []
      val next_day = day1 + 1
    in
      if next_day > day2
      then what_month (day2)::month_list
      else what_month (day1)::month_range (next_day, day2)
    end

(* ====================== *)
(* Return an option with the older date in list, or NONE if empty *)

fun oldest_date (dates: (int*int*int) list) =
  let
    val first_date = hd dates
    val next_dates = tl dates
  in
    if null next_dates then first_date
    else
      if is_older (first_date, hd next_dates)
      then oldest_date (first_date::(tl next_dates))
      else oldest_date (next_dates)
  end

fun oldest (dates: (int*int*int) list) =
  if null dates then NONE
  else
    let
      val result = oldest_date (dates)
    in
      SOME result
    end
