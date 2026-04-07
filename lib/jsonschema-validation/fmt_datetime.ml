(** Format validators: date-time, date, time (RFC 3339). *)

let is_leap_year y =
  (y mod 4 = 0 && y mod 100 <> 0) || y mod 400 = 0

let days_in_month y m =
  match m with
  | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> 31
  | 4 | 6 | 9 | 11 -> 30
  | 2 -> if is_leap_year y then 29 else 28
  | _ -> 0

let check_date s =
  (* YYYY-MM-DD *)
  String.length s = 10
  && s.[4] = '-' && s.[7] = '-'
  && (try
        let y = int_of_string (String.sub s 0 4) in
        let m = int_of_string (String.sub s 5 2) in
        let d = int_of_string (String.sub s 8 2) in
        m >= 1 && m <= 12 && d >= 1 && d <= days_in_month y m
      with _ -> false)

let check_time s =
  (* HH:MM:SS[.frac](Z | +HH:MM | -HH:MM) *)
  let len = String.length s in
  if len < 6 then false
  else
    try
      let h = int_of_string (String.sub s 0 2) in
      let m = int_of_string (String.sub s 3 2) in
      let sec_start = 6 in
      if s.[2] <> ':' || s.[5] <> ':' then false
      else if h > 23 || m > 59 then false
      else
        (* Parse seconds *)
        let i = ref sec_start in
        while !i < len && ((s.[!i] >= '0' && s.[!i] <= '9') || s.[!i] = '.') do
          incr i
        done;
        let sec_str = String.sub s sec_start (!i - sec_start) in
        if String.length sec_str = 0 then false
        else
          let sec_f = float_of_string sec_str in
          let sec_int = int_of_float sec_f in
          let sec_ok = sec_f >= 0.0 && sec_int <= 60 in
          if not sec_ok then false
          else
            (* Parse timezone *)
            if !i >= len then false
            else
              let tz_sign, tz_h, tz_m, tz_valid =
                if s.[!i] = 'Z' || s.[!i] = 'z' then
                  (1, 0, 0, !i = len - 1)
                else if s.[!i] = '+' || s.[!i] = '-' then begin
                  let tz = String.sub s !i (len - !i) in
                  if String.length tz = 6 && tz.[3] = ':' then
                    try
                      let th = int_of_string (String.sub tz 1 2) in
                      let tm = int_of_string (String.sub tz 4 2) in
                      if th >= 0 && th <= 23 && tm >= 0 && tm <= 59 then
                        let sign = if s.[!i] = '+' then 1 else -1 in
                        (sign, th, tm, true)
                      else (0, 0, 0, false)
                    with _ -> (0, 0, 0, false)
                  else (0, 0, 0, false)
                end else (0, 0, 0, false)
              in
              if not tz_valid then false
              else if sec_int < 60 then true
              else
                (* Leap second: UTC must be 23:59.
                   UTC_h = h - tz_sign * tz_h, UTC_m = m - tz_sign * tz_m *)
                let utc_m_raw = m - tz_sign * tz_m in
                let carry = if utc_m_raw < 0 then -1 else if utc_m_raw > 59 then 1 else 0 in
                let utc_m = (utc_m_raw + 60) mod 60 in
                let utc_h = ((h - tz_sign * tz_h + carry) mod 24 + 24) mod 24 in
                utc_h = 23 && utc_m = 59
    with _ -> false

let check_datetime s =
  (* Full date-time: date "T" time *)
  let len = String.length s in
  if len < 20 then false
  else
    let t_pos = 10 in
    if s.[t_pos] <> 'T' && s.[t_pos] <> 't' then false
    else
      let date_part = String.sub s 0 10 in
      let time_part = String.sub s 11 (len - 11) in
      check_date date_part && check_time time_part
