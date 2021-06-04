# Dependency License Report

This action creates a report summary of all of the transitive dependencies for a given R package and their corresponding license information.

Refer to [action.yaml](action.yaml) for information about available inputs to this action.

## Quickstart

```yaml
jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Generate license info
        uses: ./.github/actions/license-report
```
