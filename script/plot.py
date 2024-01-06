#!/usr/bin/env python3

import json
import argparse
import matplotlib.pyplot as plt


def extract_means(data):
    return data["mean"]


def plot(means):
    indexes = list(range(1, len(means) + 1))

    # Graficar la curva de puntos
    plt.figure(figsize=(8, 6))
    plt.plot(indexes, means, marker="o", linestyle="-", color="b")

    # Configuración del gráfico
    plt.title("Curva de puntos de las medias")
    plt.xlabel("Índices")
    plt.ylabel("Valores de las medias")
    plt.grid(True)

    # Mostrar la gráfica
    plt.show()


def load_json(file):
    with open("../data/" + +file, "r") as file:
        return json.load(file)


def main(arg):
    data = load_json(arg)
    means = extract_means(data)
    plot(means)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Ejemplo de pasar un argumento a main")
    parser.add_argument("arg", type=str, help="Argumento a pasar a la función main()")

    args = parser.parse_args()
    main(args.arg)
