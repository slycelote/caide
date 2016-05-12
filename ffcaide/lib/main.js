const buttons = require("sdk/ui/button/action");
const tabs = require("sdk/tabs");
const notifications = require("sdk/notifications");
const fileIO = require("sdk/io/file");
const { prefs } = require("sdk/simple-prefs");
const child_process = require("sdk/system/child_process");


var button = buttons.ActionButton({
    id: "caide-button",
    label: "Parse the problem",
    icon: {
        "16": "./icon-16.png",
        "32": "./icon-32.png",
        "64": "./icon-64.png"
    },
    onClick: parseProblemOnCurrentPage
});

function notifyError(message) {
    notifications.notify({
        "title": "caide error",
        "text": message
    });
}

function writeTextToFile(text, filename) {
    var textWriter = fileIO.open(filename, "w");
    if (!textWriter.closed) {
        textWriter.write(text);
        textWriter.close();
    }
}

function parseProblemOnCurrentPage() {
    let url = tabs.activeTab.url;
    withPageContent(pageContent => {
        if (!pageContent) {
            notifyError("Current page is not supported");
            return;
        }

        let caideRoot = (prefs.caideRoot || "").trim();
        let txtFile = caideRoot ? fileIO.join(caideRoot, ".caide", "caideExe.txt") : "";
        if (!caideRoot || !fileIO.isFile(txtFile)) {
            notifyError("Couldn't locate root caide directory. Make sure the add-on is configured correctly.");
            return;
        }

        let caideExePath = fileIO.read(txtFile).trim();
        if (!fileIO.exists(caideExePath) || !fileIO.isFile(caideExePath)) {
            notifyError("Couldn't locate caide executable");
            return;
        }

        let pagePath = fileIO.join(caideRoot, ".caide", "page.html");
        writeTextToFile(pageContent, pagePath);

        child_process.execFile(caideExePath, ['problem', url, '--from-file',  pagePath],
            {cwd: caideRoot, timeout: 3000},
            function(error, stdout, stderr) {
                if (error !== null) {
                    console.log("Error: " + error);
                    notifyError("Failed to parse the problem. See browser console for details");
                }
                console.log(stdout);
                console.log(stderr);
            });
    });
}

function withPageContent(f) {
    tabs.activeTab.attach({
        contentScript: "self.postMessage(document.body ? document.body.innerHTML : '');",
        onMessage: f
    });
}

exports.button = button;

