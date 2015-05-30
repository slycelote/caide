import httplib
import urllib
import time
import random
import hashlib
import json
import os.path
import os
from HTMLParser import HTMLParser
from htmlentitydefs import name2codepoint
from collections import defaultdict

from cfkeys import KEY, SECRET


def api_request(method_name, params):
    now = int(time.time())
    params = [(k, str(v)) for (k, v) in params]
    params += [('apiKey', KEY), ('time', now), ('lang', 'en')]
    params = sorted(params)
    url = method_name + '?' + urllib.urlencode(params)
    rand_prefix = str(random.randint(100000, 999999))
    digest = hashlib.sha512(rand_prefix + '/' + url + '#' + SECRET).hexdigest()
    params.append(('apiSig', rand_prefix + digest))

    conn = httplib.HTTPConnection('codeforces.com')
    url = 'http://codeforces.com/api/' + method_name + '?' + urllib.urlencode(params)
    conn.request('GET', url)
    response = conn.getresponse()
    print response.status, response.reason
    data = response.read()
    conn.close()
    return json.loads(data)


def get_user_info(users):
    params = [('handles', ';'.join(users))]
    return api_request('user.info', params)


def get_contest_status(contest_id):
    params = [('contestId', contest_id)]
    return api_request('contest.status', params)


def download_submission_from_url(url):
    conn = httplib.HTTPConnection('codeforces.com')
    conn.request('GET', url)
    response = conn.getresponse()
    print response.status, response.reason
    html_page = response.read()
    conn.close()

    class ProblemParser(HTMLParser):
        def __init__(self):
            HTMLParser.__init__(self)
            self.is_problem_text = False
            self.problem_text = ''

        def handle_starttag(self, tag, attrs):
            if tag == 'pre' and not self.is_problem_text:
                for (k, v) in attrs:
                    if k == 'class' and v.find('program-source') >= 0:
                        self.is_problem_text = True

        def handle_endtag(self, tag):
            if tag == 'pre':
                self.is_problem_text = False

        def handle_data(self, data):
            if self.is_problem_text:
                self.problem_text += data

        def handle_entityref(self, name):
            if self.is_problem_text:
                self.problem_text += chr(name2codepoint[name])

        def handle_charref(self, name):
            if self.is_problem_text:
                if name.startswith('x'):
                    self.problem_text += chr(int(name[1:], 16))
                else:
                    self.problem_text += chr(int(name))

    parser = ProblemParser()
    parser.feed(html_page)
    parser.close()
    return parser.problem_text


def download_submission(contest_id, submission_id):
    url = 'http://codeforces.com/contest/' + str(contest_id) + '/submission/' + str(submission_id)
    return download_submission_from_url(url)


def download_submissions(contest_id):
    contest_dir = 'cf' + str(contest_id)
    try:
        os.makedirs(contest_dir)
    except:
        pass

    print 'Getting submission list for contest id=' + str(contest_id)
    j = get_contest_status(contest_id)

    print 'Grouping...'
    grouped = defaultdict(list)  # group by author and problem
    for submission in j['result']:
        if submission['programmingLanguage'].lower().find('c++') < 0:
            continue
        if 'verdict' not in submission:
            continue
        if submission['verdict'] in ['COMPILATION_ERROR', 'SECURITY_VIOLATED', 'SKIPPED',
                                     'REJECTED']:
            continue

        party = submission['author']
        if 'teamId' in party:
            author_id = party['teamId']
        else:
            author_id = party['members'][0]['handle']
        problem_id = submission['problem']['index']
        grouped[(author_id, problem_id)].append(submission)

    print 'Downloading...'
    for submissions in grouped.viewvalues():
        submissions.sort(key=lambda s: s['creationTimeSeconds'])
        latest = submissions[-1:]
        for submission in latest:
            submission_id = submission['id']
            filename = os.path.join(contest_dir, str(submission_id) + '.cpp')
            if not os.path.isfile(filename):
                print submission_id
                code = download_submission(contest_id, submission_id)
                with open(filename, 'w') as f:
                    f.write(code)
                time.sleep(1.0)  # 1 second


def test():
    #print download_submission(534, 10976539)
    print download_submissions(547)


if __name__ == "__main__":
    test()

