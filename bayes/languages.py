#!/usr/bin/env python3
from pybbn.graph.node import BbnNode
from pybbn.graph.variable import Variable
from pybbn.graph.dag import Bbn
from pybbn.graph.edge import Edge, EdgeType
from pybbn.graph.jointree import EvidenceBuilder
from pybbn.pptc.inferencecontroller import InferenceController
import numpy as np
import sys

################################################################
## Symptoms and traits
################################################################

SYSTEM_TYPE = 'system type'
ENVIRONMENT = 'environment'
MEMORY = 'memory'
SPEED = 'speed'
PURPOSE = 'purpose'
TYPING = 'typing'
GC = 'garbage collector'
LANGUAGE = 'language'

system_type_list = ['GUI app', 'web app', 'cli', 'backend', 'machine learning']
environment_list = ['pc', 'mobile', 'cloud', 'embedded', 'cross platform']
memory_list = ['memory very important',
               'memory moderately important', 'memory not important']
speed_list = ['speed very important',
              'speed moderately important', 'speed not important']
purpose_list = ['proof of concept', 'production environment']
typing_list = ['dynamic typing', 'static typing']
gc_list = ['gc needed', 'gc not needed']


language_list = ['Python', 'Java', 'C++', 'TypeScript', 'golang', 'haskell']

python_traits = ['machine learning', 'backend', 'cli',
                 'pc', 'cloud', 'cross platform', 'embedded',
                 'memory moderately important', 'memory not important',
                 'speed not important',
                 'proof of concept',
                 'dynamic typing',
                 'gc needed', 'gc not needed']

java_traits = ['machine learning', 'backend', 'GUI app',
               'pc', 'mobile', 'cloud', 'cross platform',
               'memory moderately important', 'memory not important',
               'speed moderately important', 'speed not important',
               'production environment',
               'static typing',
               'gc needed', 'gc not needed']

cpp_traits = ['machine learning', 'backend', 'GUI app',
              'pc', 'embedded', 'cloud', 'mobile',
              'memory very important', 'memory moderately important', 'memory not important',
              'speed very important', 'speed moderately important', 'speed not important',
              'production environment',
              'static typing',
              'gc not needed']

typescript_traits = ['web app', 'backend', 'cli',
                     'pc', 'cloud', 'cross platform',
                     'memory moderately important', 'memory not important',
                     'speed not important',
                     'production environment',
                     'static typing',
                     'gc needed', 'gc not needed']

golang_traits = ['cli', 'backend',
                 'pc', 'cloud',
                 'memory very important', 'memory moderately important', 'memory not important',
                 'speed moderately important', 'speed not important',
                 'proof of concept',
                 'static typing',
                 'gc needed', 'gc not needed']

haskell_traits = ['cli', 'backend',
                  'pc', 'cloud',
                  'memory not important',
                  'speed moderately important', 'speed not important',
                  'proof of concept',
                  'static typing',
                  'gc needed', 'gc not needed']

traits = [python_traits, java_traits, cpp_traits,
          typescript_traits, golang_traits, haskell_traits]

################################################################
# Probabilities
################################################################

env_by_system_type = [
    0.3/1.5, 0.5/1.5, 0.1/1.5, 0.1/1.5, 0.5/1.5,  # GUI app
    0.4/1.3, 0.2/1.3, 0.1/1.3, 0.1/1.3, 0.5/1.5,  # web app
    0.4/1.2, 0.1/1.2, 0.3/1.2, 0.1/1.2, 0.3/1.2,  # cli
    0.2/1.3, 0.1/1.3, 0.6/1.3, 0.3/1.3, 0.1/1.3,  # backend
    0.4/1.5, 0.3/1.5, 0.5/1.5, 0.2/1.5, 0.1/1.5,  # machine learning
]
purpose_by_speed_and_mem = [
    0.1, 0.9,  # memory very important, speed very important
    0.2, 0.8,  # memory very important, speed moderately important
    0.3, 0.7,  # memory very important, speed not important
    0.4, 0.6,  # memory moderately important, speed very important
    0.5, 0.5,  # memory moderately important, speed moderately important
    0.6, 0.4,  # memory moderately important, speed not important
    0.4, 0.6,  # memory not important, speed very important
    0.7, 0.3,  # memory not important, speed moderately important
    0.8, 0.2,  # memory not important, speed not important
]

full_arrays = []
symptoms = [environment_list, purpose_list, typing_list, gc_list]

for symptom in symptoms:
    full_array = None
    for element in symptom:
        current = np.ones((len(traits)))
        for index, language_traits in enumerate(traits):
            if element in language_traits:
                current[index] = 10

        current = current / sum(current)
        if full_array is None:
            full_array = np.reshape(current, (1, current.shape[0]))
        else:
            full_array = np.concatenate(
                (full_array, np.reshape(current, (1, current.shape[0]))), axis=0)
    full_arrays.append(full_array)

final_probs = np.reshape(np.einsum('ip,jp,kp,lp->ijklp',
                                   full_arrays[0],
                                   full_arrays[1],
                                   full_arrays[2],
                                   full_arrays[3]), (-1,))

################################################################
# Graph
################################################################

system_type = BbnNode(
    Variable(0, SYSTEM_TYPE, system_type_list),
    [1 / len(system_type_list)] * len(system_type_list)
)
environment = BbnNode(
    Variable(1, ENVIRONMENT, environment_list),
    env_by_system_type
)
memory = BbnNode(
    Variable(2, MEMORY, memory_list),
    [1 / len(memory_list)] * len(memory_list)
)
speed = BbnNode(
    Variable(3, SPEED, speed_list),
    [1 / len(speed_list)] * len(speed_list)
)
purpose = BbnNode(
    Variable(4, PURPOSE, purpose_list),
    purpose_by_speed_and_mem
)
typing = BbnNode(
    Variable(5, TYPING, typing_list),
    [1 / len(typing_list)] * len(typing_list)
)
gc = BbnNode(
    Variable(6, GC, gc_list),
    [1 / len(gc_list)] * len(gc_list)
)

language = BbnNode(
    Variable(7, LANGUAGE, language_list),
    final_probs
)

bbn = Bbn()\
    .add_node(system_type)\
    .add_node(environment)\
    .add_node(language)\
    .add_node(memory)\
    .add_node(speed)\
    .add_node(purpose)\
    .add_node(typing)\
    .add_node(gc)\
    .add_edge(Edge(system_type, environment, EdgeType.DIRECTED))\
    .add_edge(Edge(environment, language, EdgeType.DIRECTED))\
    .add_edge(Edge(memory, purpose, EdgeType.DIRECTED))\
    .add_edge(Edge(speed, purpose, EdgeType.DIRECTED))\
    .add_edge(Edge(purpose, language, EdgeType.DIRECTED))\
    .add_edge(Edge(typing, language, EdgeType.DIRECTED))\
    .add_edge(Edge(gc, language, EdgeType.DIRECTED))

join_tree = InferenceController.apply(bbn)

################################################################
# User interaction
################################################################

OKGREEN = '\033[92m'
ENDC = '\033[0m'
BOLD = '\033[1m'


selected_traits = {}


def print_options(msg, values):
    print(BOLD + msg + ENDC)
    for i, val in enumerate(values):
        print(f'{i}: {val}')
    print(
        BOLD + f'enter number or \'e\' to exit: ' + ENDC, end='', flush=True)
    return sys.stdin.readline()[0]


def get_value(trait, values):
    input = print_options(f'select value for {trait}', values)
    if input == 'e':
        return False
    value = values[int(input)]
    evidence = EvidenceBuilder() \
        .with_node(join_tree.get_bbn_node_by_name(trait)) \
        .with_evidence(value, 1.0) \
        .build()
    join_tree.set_observation(evidence)
    selected_traits[trait] = value
    return True


def get_traits(trait_dict):
    input = ''
    while len(trait_dict) > 0:
        dict_keys = list(trait_dict.keys())
        print(f'{OKGREEN}selected traits:{ENDC} {selected_traits}')
        input = print_options('select next trait', dict_keys)
        if input == 'e':
            break
        trait = dict_keys[int(input)]
        used = get_value(trait, trait_dict[trait])
        if used:
            del trait_dict[trait]


def main():
    trait_dict = {
        SYSTEM_TYPE: system_type_list,
        ENVIRONMENT: environment_list,
        MEMORY: memory_list,
        SPEED: speed_list,
        PURPOSE: purpose_list,
        TYPING: typing_list,
        GC: gc_list,
    }
    join_tree.unobserve_all()
    get_traits(trait_dict)
    print(BOLD + '\nResults:' + ENDC)
    print(join_tree.get_bbn_potential(language))


if __name__ == "__main__":
    main()
