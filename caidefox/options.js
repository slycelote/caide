"use strict";

function onError(error) {
    console.error(`Error: ${error}`);
}

function saveOptions(e) {
    e.preventDefault();
    browser.storage.local.set({
        caideDir: document.querySelector("#dir").value
    });
}

function restoreOptions() {
    function setCurrentChoice(options) {
        if (Array.isArray(options)) {
            // Prior to FF 52, this is a one-element array.
            options = options[0];
        }
        document.querySelector("#dir").value = options.caideDir || "";
    }

    browser.storage.local.get(["caideDir"]).then(setCurrentChoice, onError);
}

document.addEventListener("DOMContentLoaded", restoreOptions);
document.querySelector("form").addEventListener("submit", saveOptions);

