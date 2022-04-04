class Timer:

    import gc
    import sys
    import timeit
    import functools

    class Result:

        import sys
        import math

        def __init__(self, times, loops, runs, precision=3):
            self._times = times
            self._loops = loops
            self._runs = runs
            self._timings = [t / self._loops for t in self._times]
            self._precision = precision

        @property
        def mean(self):
            return self.math.fsum(self._times) / len(self._timings)

        @property
        def stddev(self):
            return (self.math.fsum((x - self.mean) ** 2 for x in self._timings) / len(self._timings)) ** 0.5

        def _format_time(self, timespan, precision=3):
            if timespan >= 60.0:
                parts = [("d", 60*60*24),("h", 60*60),("min", 60), ("s", 1)]
                time = []
                leftover = timespan
                for suffix, length in parts:
                    value = int(leftover / length)
                    if value > 0:
                        leftover = leftover % length
                        time.append(u'%s%s' % (str(value), suffix))
                    if leftover < 1:
                        break
                return " ".join(time)
            units = [u"s", u"ms",u'us',"ns"] # the save value   
            if hasattr(self.sys.stdout, 'encoding') and self.sys.stdout.encoding:
                try:
                    u'\xb5'.encode(self.sys.stdout.encoding)
                    units = [u"s", u"ms",u'\xb5s',"ns"]
                except:
                    pass
            scaling = [1, 1e3, 1e6, 1e9]

            if timespan > 0.0:
                order = min(-int(self.math.floor(self.math.log10(timespan)) // 3), 3)
            else:
                order = 3
            return u"%.*g %s" % (precision, timespan * scaling[order], units[order])

        def __str__(self):
            pm = '+-'
            if hasattr(self.sys.stdout, 'encoding') and self.sys.stdout.encoding:
                try:
                    u'\xb1'.encode(self.sys.stdout.encoding)
                    pm = u'\xb1'
                except:
                    pass
            return "{mean} {pm} {std} per loop (mean {pm} std. dev. of {runs} run{run_plural}, {loops:,} loop{loop_plural} each)".format(
                pm=pm,
                runs=self._runs,
                loops=self._loops,
                loop_plural="" if self._loops == 1 else "s",
                run_plural="" if self._runs == 1 else "s",
                mean=self._format_time(self.mean, self._precision),
                std=self._format_time(self.stddev, self._precision),
            )

    def __init__(self, func, *args, **kwargs):
        self._func = func
        self._args = args
        self._kwargs = kwargs

    def _timeit(self, loops):
        return self.timeit.timeit(
            self.functools.partial(
                self._func, *self._args, **self._kwargs
                ),
            number=loops
        )

    def run(self, runs=1, loops=1):
        gc_enabled = self.gc.isenabled()
        times = []
        try:
            for i in range(runs):
                times.append(self._timeit(loops))
        finally:
            if gc_enabled:
                self.gc.enable()
        return self.Result(times, loops, runs)

    


if __name__ == "__main__":
    import time
    def func(a, kw=2):
        time.sleep(2)
        print(a)
        print(kw)

    tm = Timer(func, "hello")
    res = tm.run()
    print(res)
    print(str(res))
        
