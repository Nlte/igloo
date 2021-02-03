select timediff(now(),convert_tz(now(),@@session.time_zone,'+00:00'));
