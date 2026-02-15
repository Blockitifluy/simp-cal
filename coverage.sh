#!/bin/bash

cargo tarpaulin --out Html && open tarpaulin-report.html
