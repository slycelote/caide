"""
Parses plan.json and draws dependency graph
"""

import json, sys

def main():
    with open(sys.argv[1], 'r') as f:
        j = json.load(f)
    names = {}
    edges = {}
    for p in j['install-plan']:
        pid = p['id']
        names[pid] = p['pkg-name']
        to_process = [p]
        if 'components' in p:
            to_process.extend(p['components'].values())
        edges[pid] = []
        for component in to_process:
            if 'depends' in component:
                edges[pid].extend(component['depends'])

    print('digraph {')
    ignored = set([
        # shipped with ghc
        'array', 'directory', 'ghc-prim', 'template-haskell', 'exceptions', 'mtl', 'stm', 'ghc-heap', 'pretty', 'base', 'haskeline',
        'integer-gmp', 'ghc-boot-th', 'ghci', 'libiserv', 'hpc', 'parsec', 'text', 'xhtml', 'ghc', 'unix', 'time', 'deepseq',
        'bytestring', 'Cabal', 'containers', 'binary', 'ghc-compact', 'ghc-boot', 'transformers', 'filepath', 'process', 'terminfo',
        'Win32', 'rts'
    ])

    red = set(['aeson']) # In addition, color red transitive dependencies of these
    red_queue = list(k for k in edges if names[k] in red)
    while len(red_queue) > 0:
        k = red_queue.pop()
        for v in edges[k]:
            if names[v] not in red:
                red.add(names[v])
                red_queue.append(v)

    for k in edges:
        if names[k] in ignored:
            continue
        if names[k] in red:
            print('"' + names[k] + '" [color=red]')
        for v in edges[k]:
            if names[v] in ignored:
                continue
            print('"' + names[k] + '" -> "' + names[v] + '"')
    print('}')

if __name__=='__main__':
    main()
