(* check if a given date A (Y-M-D) (int*int*int) is older than date B *)

fun is_older (date1: int*int*int, date2: int*int*int) =
  let
    fun year (date: (int*int*int)) = #1 date
    fun month (date: (int*int*int)) = #2 date
    fun day (date: (int*int*int)) = #3 date
  in
    year(date1) < year(date2) orelse
    month(date1) < month(date2) orelse
    day(date1) < day(date2)
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
    check_dates (dates)
  end

(* ====================== *)
(* Check how many dates are in any of the given months *)

fun number_in_months (dates: (int*int*int) list, months: int list) =
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
    check_dates (dates)
  end

(* ====================== *)
(* Return dates that are in any of the given months *)

fun dates_in_months (dates: (int*int*int) list, months: int list) =
  let
    val base_list = dates_in_month (dates, hd months)
  in
    if null (tl months)
    then base_list
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
