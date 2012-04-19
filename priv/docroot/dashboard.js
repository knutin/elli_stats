$(document).ready(function() {
    var e = new EventSource("stats/stream");
    e.addEventListener("open", function (event) {
        console.log("open");
    });

    e.onmessage = function (event) {
        var data = $.parseJSON(event.data);
        //console.log(data);
        $("#requests tbody").html('');

        _.each(data, function (v, k) {
            $("#requests tbody:last").append("<tr>" +
                                             "<td>" + k + "</td>" +
                                             "<td>" + v['frequency'] + "</td>" +
                                             "<td>" + v['mean'] + "</td>" +
                                             "</tr>");
        });

    };
});