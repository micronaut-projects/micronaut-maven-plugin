#!/usr/bin/env python3
"""Generate balanced GitHub Actions matrix shards for Maven Invoker ITs.

The Maven Invoker test list is derived from the existing src/it/*/pom.xml
projects so new IT directories are automatically picked up by CI.
"""

from __future__ import annotations

import argparse
import json
import re
from pathlib import Path


def setup_project_names(harness_pom: Path) -> set[str]:
    """Return setup project names declared by maven-invoker-plugin."""
    if not harness_pom.exists():
        return set()
    text = harness_pom.read_text(encoding="utf-8")
    return {
        Path(match).parent.name
        for match in re.findall(r"<setupInclude>\s*([^<]+?)\s*</setupInclude>", text)
    }


def invoker_project_names(projects_dir: Path) -> list[str]:
    return sorted(path.parent.name for path in projects_dir.glob("*/pom.xml"))


def test_weight(name: str, projects_dir: Path) -> int:
    """Estimate runtime cost without hard-coding individual test names.

    The weights intentionally use broad feature patterns rather than specific
    scenario names. They keep native-image and Docker-heavy scenarios spread
    across shards, while still allowing newly added ITs to be assigned
    automatically.
    """
    properties = projects_dir / name / "invoker.properties"
    props = properties.read_text(encoding="utf-8") if properties.exists() else ""
    haystack = f"{name}\n{props}".lower()

    weight = 10
    if "docker-native" in haystack or "native-image" in haystack or "-pgraalvm" in haystack:
        weight = max(weight, 180)
    elif "docker-crac" in haystack:
        weight = max(weight, 80)
    elif "docker" in haystack:
        weight = max(weight, 35)

    if "test-resources" in haystack:
        weight = max(weight, 30)
    if "aot" in haystack:
        weight = max(weight, 25)
    if "openapi" in haystack or "jsonschema" in haystack or "configuration-validation" in haystack:
        weight = max(weight, 12)

    return weight


def build_shards(projects_dir: Path, shard_count: int) -> list[dict[str, str]]:
    harness_pom = projects_dir.parent.parent / "pom.xml"
    setup_names = setup_project_names(harness_pom)
    projects = invoker_project_names(projects_dir)
    tests = [name for name in projects if name not in setup_names]

    buckets: list[dict[str, object]] = [
        {"name": f"shard-{index + 1}", "weight": 0, "tests": []} for index in range(shard_count)
    ]

    weighted_tests = sorted(
        ((test_weight(name, projects_dir), name) for name in tests),
        key=lambda item: (-item[0], item[1]),
    )

    for weight, name in weighted_tests:
        bucket = min(buckets, key=lambda candidate: (candidate["weight"], candidate["name"]))
        bucket["tests"].append(name)
        bucket["weight"] += weight

    matrix = []
    for bucket in buckets:
        selected_tests = [*sorted(setup_names), *bucket["tests"]]
        matrix.append(
            {
                "name": str(bucket["name"]),
                "tests": ",".join(selected_tests),
            }
        )
    return matrix


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--projects-dir", default="micronaut-maven-integration-tests/src/it")
    parser.add_argument("--shards", type=int, default=5)
    parser.add_argument("--github-output", default=None)
    args = parser.parse_args()

    matrix = build_shards(Path(args.projects_dir), args.shards)
    payload = json.dumps(matrix, separators=(",", ":"))

    if args.github_output:
        with open(args.github_output, "a", encoding="utf-8") as output:
            output.write(f"shards={payload}\n")
    else:
        print(payload)


if __name__ == "__main__":
    main()
