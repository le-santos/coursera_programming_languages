(* check if a given date A (Y-M-D) (int*int*int) is older than date B *)

fun is_older (date1: int*int*int, date2: int*int*int) =
  let
    fun is_minor (a: int, b: int) = a < b
    fun year (date: (int*int*int)) = #1 date
    fun month (date: (int*int*int)) = #2 date
    fun day (date: (int*int*int)) = #3 date
  in
    is_minor (year(date1), year(date2)) orelse
    is_minor (month(date1), month(date2)) orelse
    is_minor (day(date1), day(date2))
  end

(* ====================== *)
(* check how many dates are in the given month *)

fun number_in_month (dates: (int*int*int) list, month: int) =
  let
    fun compare_month (date_month: (int*int*int), month) =
      if (#2 date_month) = month then 1 else 0

    fun check_dates (dates) =
      let
        val first_result = compare_month((hd dates), month)
      in
        if null (tl dates)
        then first_result
        else first_result + check_dates(tl dates)
      end
  in
    check_dates (dates)
  end

(* ====================== *)
(* Check how many dates are in any of the given months *)
fun number_in_months (dates: (int*int*int) list, months: int list) =
  let
    val initial_count = number_in_month (dates, hd months)
  in
    if null (tl months)
    then initial_count
    else initial_count + number_in_months (dates, tl months)
  end
