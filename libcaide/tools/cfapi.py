from cfkeys import KEY, SECRET
import httplib, urllib, time, random, hashlib, json, os.path
from HTMLParser import HTMLParser
from htmlentitydefs import name2codepoint
from collections import defaultdict


def apiRequest(method_name, params):
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
    return apiRequest('user.info', params)

def get_contest_status(contestId):
    params = [('contestId', contestId)]
    return apiRequest('contest.status', params)


def download_submission_from_url(url):
    conn = httplib.HTTPConnection('codeforces.com')
    conn.request('GET', url)
    response = conn.getresponse()
    print response.status, response.reason
    data = response.read()
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
    parser.feed(data)
    parser.close()
    return parser.problem_text

def download_submission(contestId, submissionId):
    url = 'http://codeforces.com/contest/'+str(contestId) + '/submission/'+str(submissionId)
    return download_submission_from_url(url)

def download_submissions(contestId):
    print 'Getting submission list for contest id=' + str(contestId)
    j = get_contest_status(contestId)

    print 'Grouping...'
    grouped = defaultdict(list) # group by author and problem
    for submission in j['result']:
        if submission['programmingLanguage'].lower().find('c++') < 0:
            continue
        if 'verdict' not in submission:
            continue
        if submission['verdict'] in ['COMPILATION_ERROR','SECURITY_VIOLATED','SKIPPED','REJECTED']:
            continue

        party = submission['author']
        if 'teamId' in party:
            authorId = party['teamId']
        else:
            authorId = party['members'][0]['handle']
        problemId = submission['problem']['index']
        grouped[(authorId, problemId)].append(submission)

    j = None

    print 'Downloading...'
    for submissions in grouped.viewvalues():
        submissions.sort(key=lambda s: s['creationTimeSeconds'])
        latest = submissions[-1:]
        for submission in latest:
            submissionId = submission['id']
            filename = os.path.join('cf', str(submissionId)+'.cpp')
            if not os.path.isfile(filename):
                print submissionId
                code = download_submission(contestId, submissionId)
                with open(filename, 'w') as f:
                    f.write(code)
                time.sleep(1.0) # 1 second

def test():
    print download_submission(534, 10976539)

if __name__ == "__main__":
    test()

