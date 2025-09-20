"""
Parses dist-newstyle/cache/plan.json and draws dependency graph
"""

import json, sys

def main():
    with open(sys.argv[1], 'r') as f:
        j = json.load(f)
    names = {}
    edges = {}
    preinstalled = set() # shipped with ghc
    for p in j['install-plan']:
        pid = p['id']
        names[pid] = p['pkg-name']
        if p['type'] == 'pre-existing':
            preinstalled.add(names[pid])
        to_process = [p]
        if 'components' in p:
            to_process.extend(p['components'].values())
        edges[pid] = []
        for component in to_process:
            if 'depends' in component:
                edges[pid].extend(component['depends'])

    # print(preinstalled, file=sys.stderr)
    print('digraph {')

    red = set(['aeson']) # In addition, color red transitive dependencies of these
    red_queue = list(k for k in edges if names[k] in red)
    while len(red_queue) > 0:
        k = red_queue.pop()
        for v in edges[k]:
            if names[v] not in red:
                red.add(names[v])
                red_queue.append(v)

    for k in edges:
        if names[k] in preinstalled:
            continue
        if names[k] in red:
            print('"' + names[k] + '" [color=red]')
        for v in edges[k]:
            if names[v] in preinstalled:
                continue
            print('"' + names[k] + '" -> "' + names[v] + '"')
    print('}')

if __name__=='__main__':
    main()
