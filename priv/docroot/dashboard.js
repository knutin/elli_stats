$(document).ready(function() {
    var e = new EventSource("stats/stream");
    e.addEventListener("open", function (event) {
    });

    e.onmessage = function (event) {
        var data = $.parseJSON(event.data);
        $("#requests tbody").html('');

        _.each(data['timings'], function (v, k) {
            if(k == "_total") { return };
            append_row(k, v);
        });
        append_row("Total", data["timings"]["_total"]);

    };
});

function append_row(k, v) {
    if(!v) { return };
    var rps = v['observations'];

    var mean = format_us(v['mean']) + " ms";
    var sd = format_us(v['sd']) + " ms";
    var p99 = format_us(v['p99']) + " ms";

    $("#requests tbody:last").append("<tr>" +
                                     "<td>" + k + "</td>" +
                                     "<td>" + rps + "</td>" +
                                     "<td>" + mean + "</td>" +
                                     "<td>" + sd + "</td>" +
                                     "<td>" + p99 + "</td>" +
                                     "</tr>");
}


function format_us(us) {
    return (us / 1000).toFixed(4);
}