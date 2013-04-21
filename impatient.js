var buffer = window.location.pathname.split('/')[3];
var max_period = 60000;
var min_period = 1000;
var next_period = min_period;
var alpha = 1.2;
var current_id = '-1';
$('head').append($('<title/>').text(decodeURI(buffer)));

var nextTimeout = function() {
    var next = next_period;
    next_period = Math.min(max_period, next_period * alpha);
    return next;
};

var resetTimeout = function() {
    next_period = min_period;
};

var refresh = function() {
    function update(count) {
        resetTimeout();
        current_id = count;
        $('#content').attr('src', '/imp/buffer/' + buffer + '/?' + $.param({
            id: current_id
        }));
        refresh();
    }
    $.get("/imp/update/" + buffer, {
        id: current_id
    }, update).error(function() {
        setTimeout(refresh, nextTimeout());
    });
};

$(document).ready(refresh);
