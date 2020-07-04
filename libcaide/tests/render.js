"use strict";
var fs = require('fs')
var system = require('system');

var url = system.args[1];
var filePath = system.args[2];

var page = require('webpage').create();
page.open(url, function() {
    setTimeout(function() {
        var html = page.evaluate(function() {
            return document.body.innerHTML; // body.innerHTML is what CHelper extension sends
        });
        fs.write(filePath, html, 'w');
        phantom.exit();
    }, 3000);
});

