fun is_older(d1: int*int*int, d2: int*int*int) =
  if #1 d1 = #1 d2
  then
    if #2 d1 = #2 d2
    then #3 d1 < #3 d2
    else #2 d1 < #2 d2
  else #1 d1 < #1 d2

fun number_in_month(dates: (int*int*int) list, month: int) =
  if null dates
  then 0
  else let val counter = if #2 (hd dates) = month then 1 else 0
       in counter + number_in_month(tl dates, month)
       end

fun number_in_months(dates: (int*int*int) list, months: int list) =
  if null months
  then 0
  else let val counter = number_in_month(dates, hd months)
       in counter + number_in_months(dates, tl months)
       end

fun dates_in_month(dates: (int*int*int) list, month: int) =
  if null dates
  then []
  else let val result = dates_in_month(tl dates, month)
       in
         if #2 (hd dates) = month
         then (hd dates)::result
         else result
       end

fun dates_in_months(dates: (int*int*int) list, months: int list) = 
  if null months
  then []
  else let val result = dates_in_month(dates, hd months)
       in result @ dates_in_months(dates, tl months)
       end

fun get_nth(strings: string list, n: int) = 
  if n = 1
  then hd strings
  else get_nth(tl strings, n-1)

fun date_to_string(date: int*int*int) = 
  let
    val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in get_nth(months, #2 date) ^ " " ^ Int.toString(#3 date) ^ ", " ^ Int.toString(#1 date) 
  end

fun number_before_reaching_sum(sum: int, values: int list) =
  if sum <= 0
  then ~1
  else number_before_reaching_sum(sum - hd values, tl values) + 1

fun what_month(day: int) = 
  let val days_in_month = [31,28,31,30,31,30,31,31,30,31,30,31]
  in number_before_reaching_sum(day, days_in_month) + 1
  end

fun month_range(d1: int, d2: int) =
  if d1 > d2
  then []
  else what_month(d1)::month_range(d1+1, d2)

fun oldest(dates: (int*int*int) list) = 
  if null dates
  then NONE
  else let 
         fun oldest_nonempty(dates: (int*int*int) list) = 
           if null (tl dates)
           then hd dates
           else let val old = oldest_nonempty(tl dates)
                in
                  if is_older(old, hd dates)
                  then old
                  else hd dates
                end
       in 
         SOME (oldest_nonempty(dates))
       end

fun remove_duplicates(duplicates: int list, distinct: int list) =
  if null duplicates
  then distinct
  else
    let fun not_in_list(values: int list, value: int) =
          if null values
          then true
          else 
            if hd values = value
            then false
            else not_in_list(tl values, value)
    in
      if not_in_list(distinct, hd duplicates)
      then remove_duplicates(tl duplicates, distinct @ [hd duplicates])
      else remove_duplicates(tl duplicates, distinct)
    end

fun number_in_months_challenge(dates: (int*int*int) list, months: int list) =
  number_in_months(dates, remove_duplicates(months,[]))

fun dates_in_months_challenge(dates: (int*int*int) list, months: int list) =
  dates_in_months(dates, remove_duplicates(months,[]))

fun reasonable_date(date: int*int*int) = 
  let
    val year = #1 date
    val month = #2 date
    val day = #3 date
    val leap_year = (year mod 400 = 0 orelse year mod 4 = 0) andalso (year mod 100 <> 0)
  in
    if year < 1 orelse month < 1 orelse month > 12 orelse day < 1 orelse day > 31
    then false
    else 
      if (month = 4 orelse month = 6 orelse month = 9 orelse month = 11) andalso day = 31
      then false
      else if month = 2
           then if day <= 28 orelse (day = 29 andalso leap_year)
                then true
                else false
      else true
  end



