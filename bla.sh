#!/bin/bash
cpp -undef -Wno-invalid-pp-token $1 | sed -e s/_##/_/g
