#!/usr/bin/env python3

import re
import json
import argparse
import matplotlib.pyplot as plt


def extract_means(data):
    return [record["mean"] for record in data["results"]]


def get_labels(object_type):
    return {
        "a": ("Subsets", "Items in the set"),
        "c": ("Dyck Paths", "Nodes in the path"),
        "f": ("Combinations", "Items in the set"),
        "b": ("Set Partitions", "Items in the set"),
        "d": ("Complete Linked Diagrams", "Arcs in the diagram"),
        "g": ("Labeled Connected Graphs", "Points in the graph"),
        "s": ("Irreducible Linked Diagrams", "Arcs in the diagram"),
    }[object_type]


def show_curve_for_ge(indexes, object_type, algorithm_type):
    if object_type == "g" and algorithm_type == "eumeration":
        estimations = [x for x in indexes]
        plt.plot(indexes, estimations, ".", color="#3cb31e")


def plot(means, object_type, algorithm_type):
    indexes = list(range(1, len(means) + 1))
    name_label, x_label = get_labels(object_type)

    plt.figure(figsize=(8, 6))
    plt.plot(indexes, means, ".", color="#ff3399")

    plt.title("Algorithm behavior to " + algorithm_type + " " + name_label)
    plt.xlabel(x_label)
    plt.ylabel("Time (seconds)")
    plt.grid(True, color="#e0e0e0", linestyle="--", linewidth=1)

    show_curve_for_ge(indexes, object_type, algorithm_type)
    plt.show()


def get_algorithm_type(path):
    match = re.search(r"([a-z]+)/([a-z]+)", path)
    if match != None:
        return match.group(2)
    else:
        return None


def get_object_type(path):
    match = re.search(r"\d+([a-z]+)\d+", path)
    if match != None:
        return match.group(1)
    else:
        return None


def load_json(path_to_file):
    with open(path_to_file, "r") as path_to_file:
        return json.load(path_to_file)


def main(path):
    data = load_json(path)
    means = extract_means(data)
    object_type = get_object_type(path)
    algorithm_type = get_algorithm_type(path)
    plot(means, object_type, algorithm_type)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Object path to plot")
    parser.add_argument("path", type=str)

    args = parser.parse_args()
    main(args.path)
