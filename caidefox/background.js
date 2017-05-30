"use strict";

function onError(error) {
    console.error(`Error: ${error}`);
}

function parseProblem() {
    function sendToTabs(tabs) {
        for (let activeTab of tabs) {
            browser.tabs.sendMessage(activeTab.id, {}).then(response => {
                browser.storage.local.get(["caideDir"]).then(options => {
                    if (Array.isArray(options)) {
                        options = options[0];
                    }
                    response['caideDir'] = options['caideDir'];
                    // console.log("Response: ");
                    // console.log(response);
                    browser.runtime.sendNativeMessage("caide", response);
                });

            });
        }
    }

    browser.tabs.executeScript(null, {
        file: "/content-script.js"
    });
    browser.tabs.query({active: true, currentWindow: true}).then(sendToTabs).catch(onError);
}

browser.browserAction.onClicked.addListener(parseProblem);

