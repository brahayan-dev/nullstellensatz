#!/usr/bin/env python3

import json
import argparse
import matplotlib.pyplot as plt


def extract_means(data):
    return [record["mean"] for record in data["results"]]


def plot(means):
    indexes = list(range(1, len(means) + 1))

    plt.figure(figsize=(8, 6))
    plt.plot(indexes, means, marker="o", linestyle="-", color="b")

    plt.title("Algorithm behavior to generate LCG")
    plt.xlabel("Points in the graph")
    plt.ylabel("Time (seconds)")
    plt.grid(True)

    plt.show()


def load_json(path_to_file):
    with open(path_to_file, "r") as path_to_file:
        return json.load(path_to_file)


def main(path):
    data = load_json(path)
    means = extract_means(data)
    plot(means)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Object path to plot")
    parser.add_argument("path", type=str)

    args = parser.parse_args()
    main(args.path)
