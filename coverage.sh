#!/bin/bash

cargo tarpaulin --out Html --exclude-files src/main.rs && open tarpaulin-report.html
