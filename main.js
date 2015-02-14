;(function(ls) {

    function genEntry (k) {
        var e = JSON.parse(ls[k])
        //e.expire = new Date(e.expire)
        e.key = k
        e.isPast = e.expire && (e.expire <= new Date().getTime())
        return e
    }

    function isCxKey (k) {
        return k.indexOf('cx') === 0
    }

    function getLS (keys) {
        var xs = []

        if (!keys)
            keys = Object.keys(ls)

        xs = keys.filter(isCxKey).map(genEntry)

        return JSON.stringify(xs)
    }

    var todomvc = Elm.fullscreen(Elm.Main, { getStorage: getLS()});

    todomvc.ports.refreshStorage.subscribe(function(flag) {
        flag = flag.toLowerCase()
        console.debug("refresh ls event : ", flag)
        if (flag === "debug") return
            
        if (flag === "clear") {
            ls.clear()
        }
        todomvc.ports.getStorage.send(getLS())
    })


})(window.localStorage);
