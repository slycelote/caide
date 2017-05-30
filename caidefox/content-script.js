"use strict";

function onError(error) {
    console.error(`Error: ${error}`);
}

function download(url) {
    return new Promise((resolve, reject) => {
        var xhr = new XMLHttpRequest();
        xhr.open("GET", url);
        xhr.onload = function() {
            if (xhr.status == 200) {
                resolve(xhr);
            } else {
                reject(xhr.statusText);
            }
        };
        xhr.onerror = function() {
            reject(Error('There was a network error'));
        };
        xhr.send();
    });
}

function getDefaultAsync() {
    return Promise.resolve({
        'document': document.documentElement.outerHTML,
        'url': document.URL,
        'problemType': 'default'
    });
}

function getDcjAsync() {
    let problemDivs = document.querySelectorAll("*[class='dsb-content-pages']");
    if (problemDivs.length == 0)
        return null;
    let problemDiv = Array.prototype.filter.call(problemDivs,
            div => div.id.startsWith('dsb-problem-page') && div.style.display == 'block')[0];

    let csrfToken = encodeURIComponent(problemDiv.querySelector("input[name='csrfmiddlewaretoken']").value);
    // long numeric Id of the problem
    let problem = problemDiv.querySelector("input[name='problem']").value;
    // 0-based index.
    let problemIndex = problemDiv.querySelector("input[name='problem_index']").value;
    let problemTitle = document.querySelector(
        "#dsb-problem-status-wrapper" + problemIndex + " div[class='dsb-status-problem-title']").innerHTML;

    // https://code.google.com/codejam/contest/8314486/dashboard/do/pancakes.h?cmd=GetSampleInput&problem=5694847428067328&sample_id=0&language=CPP&filename=pancakes.h&agent=website&csrfmiddlewaretoken=MWRkNzg1N2IwMjU1MTA4NThkYTRiMzJhMDAzNTA5NjJ8fDE0OTUxMjIzNDU3MzQ4NjA%3D
    let docUrl = new URL(document.URL);
    let url = docUrl.origin + docUrl.pathname + `/do/${problemTitle}.h?cmd=GetSampleInput&problem=${problem}`;

    let allSampleInputs = problemDiv.querySelectorAll("a[class='dsb-download-sample-link']");
    let cppSampleInputs = Array.prototype.filter.call(allSampleInputs, a => a.id.toLowerCase().endsWith("cpp"));
    let downloaders = [];
    for (let i = 0; i < cppSampleInputs.length; ++i) {
        let sampleInputUrl = url +
            `&sample_id=${i}&language=CPP&filename=${problemTitle}.h&agent=website&csrfmiddlewaretoken=${csrfToken}`;
        downloaders.push(download(sampleInputUrl));
    }

    return Promise.all(downloaders).then(sampleInputs => { return {
        'problemTitle': problemTitle,
        'sampleInputs': sampleInputs.map(xhr => xhr.responseText),
        'document': document.documentElement.outerHTML,
        'url': document.URL,
        'problemType': 'dcj'
    }});
}

function getPageDataAsync() {
    return getDcjAsync() || getDefaultAsync();
}

function returnPageData(request, sender, sendResponse) {
    browser.runtime.onMessage.removeListener(returnPageData);
    try {
        return getPageDataAsync().catch(onError);
    } catch (e) {
        // onError(e);
        return Promise.reject(e);
    }
}

browser.runtime.onMessage.addListener(returnPageData);

