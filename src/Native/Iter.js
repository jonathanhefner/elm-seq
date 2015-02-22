Elm.Native.Iter = {};
Elm.Native.Iter.make = function(localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Iter = localRuntime.Native.Iter || {};
    if (localRuntime.Native.Iter.values)
    {
        return localRuntime.Native.Iter.values;
    }
    if ('values' in Elm.Native.Iter)
    {
        return localRuntime.Native.Iter.values = Elm.Native.Iter.values;
    }

    function iterList(xs, reducer, acc) {
        var i = 0;
        while (acc.ctor !== 'Done' && xs.ctor !== '[]') {
          acc = A3(reducer, i, xs._0, acc);
          xs = xs._1;
          i++;
        }
        return acc;
    }

    Elm.Native.Iter.values = {
        iterList: F3(iterList),
    };
    return localRuntime.Native.Iter.values = Elm.Native.Iter.values;
};
