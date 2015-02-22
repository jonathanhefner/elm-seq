Elm.Native.Seq = {};
Elm.Native.Seq.make = function(localRuntime) {
    localRuntime.Native = localRuntime.Native || {};
    localRuntime.Native.Seq = localRuntime.Native.Seq || {};
    if (localRuntime.Native.Seq.values)
    {
        return localRuntime.Native.Seq.values;
    }
    if ('values' in Elm.Native.Seq)
    {
        return localRuntime.Native.Seq.values = Elm.Native.Seq.values;
    }

    var Utils = Elm.Native.Utils.make(localRuntime);
    var Nil = Utils.Nil;

    function elide(seq) {
        return seq;
    }

    function reify(ignored, seq) {
        return seq;
    }

    function unsafeReverse(list) {
        var curr = list;
        var next;
        var tail = Nil;

        while (curr.ctor !== '[]') {
            next = curr._1;
            curr._1 = tail;
            tail = curr;
            curr = next;
        }

        return tail;
    }

    function toList(toReverseList, seq) {
        return unsafeReverse(toReverseList(seq));
    }

    Elm.Native.Seq.values = {
        elide: elide,
        reify: F2(reify),
        toList: F2(toList),
    };
    return localRuntime.Native.Seq.values = Elm.Native.Seq.values;
};
