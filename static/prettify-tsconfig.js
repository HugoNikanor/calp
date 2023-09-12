#!/usr/bin/env node

/*
 * Script to normalize tsconfig.json.
 */

const { parse, stringify, assign } = require('comment-json')
const fs = require('fs')

const filename = 'tsconfig.json'

const obj = parse(fs.readFileSync(filename).toString())
const output = stringify(obj, null, 4)

fs.writeFileSync(filename, output)
