"use strict";
var fs = require('fs')
var system = require('system');

var url = system.args[1];
var filePath = system.args[2];

var page = require('webpage').create();
page.open(url, function() {
    var html = page.evaluate(function() {
        return document.documentElement.outerHTML;
    });
    fs.write(filePath, html, 'w');
    phantom.exit();
});

