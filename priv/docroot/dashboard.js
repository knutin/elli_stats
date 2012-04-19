$(document).ready(function() {
    var e = new EventSource("stats/stream");
    e.addEventListener("open", function (event) {
        console.log("open");
    });

    e.onmessage = function (event) {
        var data = $.parseJSON(event.data);
        console.log(data);
        $("#requests tbody").html('');

        _.each(data, function (v, k) {
            if(k == "_total") { return }
            append_row(k, v);
        });

        append_row("Total", data["_total"]);

    };
});

function append_row(k, v) {
    var rps = v['observations'] / 5;
    var mean = format_us(v['mean']) + " ms";

    var percentiles =
        "95th %: " + format_us(v['p95']) + " ms, " +
        "99th %: " + format_us(v['p99']) + " ms, " +
        "99.9th %: " + format_us(v['p999']) + " ms";

    $("#requests tbody:last").append("<tr>" +
                                     "<td>" + k + "</td>" +
                                     "<td>" + rps + "</td>" +
                                     "<td>" + mean + "</td>" +
                                     "<td>" + percentiles + "</td>" +
                                     "</tr>");
}


function format_us(us) {
    return (us / 1000).toFixed(4);
}