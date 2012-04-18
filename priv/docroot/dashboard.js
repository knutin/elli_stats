$(document).ready(function() {
    var e = new EventSource("stats/stream");
    e.addEventListener("open", function (event) {
        console.log("open");
    });

    e.onmessage = function (event) {
        var data = $.parseJSON(event.data);
        $("#rps").html(data["request"]);
    };
});