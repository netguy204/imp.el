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

var frameToDocument = function(iframe) {
    return (iframe.contentDocument) ? iframe.contentDocument : iframe.Document;
};

var printIframe = function(data) {
    var iframeJQ = $('#content');
    var doc = frameToDocument(iframeJQ[0]);
    doc.open();
    doc.write(data);
    doc.close();
};

var setIframe = function(count, newText) {
    if(!count) {
        // error parsing client result
        printIframe('0', 'error parsing the response from emacs');
    } else {
        current_id = count;
        printIframe(newText);
        lastText = newText;
    }
};

var refresh = function() {
    var url = "/imp/buffer/" + buffer;

    var gotData = function(data, status, xhr) {
        resetTimeout();
        setIframe(xhr.getResponseHeader("X-Imp-Count"), data);
        //$('#content').attr('src', url + '?id=-1');
        refresh();
    };

    var errorRetry = function() {
        setTimeout(refresh, nextTimeout());
    };

    $.get(url + '?id=' + current_id, gotData).error(errorRetry);
};

$(document).ready(function() {
    var iframeJQ = $('#content');
    iframeJQ.load(function() {
        // now we need to tweak the stylesheet links so that firefox will refresh them properly
        $('link', frameToDocument(iframeJQ[0])).each(function(index, el) {
            var href = $(el).attr('href');
            // don't refresh the random data:text stylesheets that chrome inserts
            if(href && href.indexOf('data:text') != 0) {
                $(el).attr('href', $(el).attr('href') + '?' + new Date().getTime());
            }
        });
    });

    refresh();
});
