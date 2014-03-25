#!/usr/bin/env lsc
# options are accessed as argv.option

_       = require('underscore')
_.str   = require('underscore.string');
moment  = require 'moment'
fs      = require 'fs'
color   = require('ansi-color').set
os      = require('os')
shelljs = require('shelljs')
table   = require('ansi-color-table')

_.mixin(_.str.exports());
_.str.include('Underscore.string', 'string');

name        = "Device Query"
description = "OpenCL device query for dummies"
author      = "Vittorio Zaccaria"
year        = "2013"

src = __dirname
otm = if (os.tmpdir?) then os.tmpdir() else "/var/tmp"
cwd = process.cwd()

setup-temporary-directory = ->
    name = "tmp_#{moment().format('HHmmss')}_tmp"
    dire = "#{otm}/#{name}" 
    shelljs.mkdir '-p', dire
    return dire

remove-temporary-directory = (dir) ->
    shelljs.rm '-rf', dir 
    
usage-string = """

#{color(name, \bold)}. #{description}
(c) #author, #year

Usage: #{name} [--option=V | -o V] 
"""

require! 'optimist'

argv     = optimist.usage(usage-string,

              help:
                alias: 'h', description: 'this help', default: false

                         ).boolean(\h).argv


if(argv.help)
  optimist.showHelp()
  return

command = argv._

WebCL = require('webcl-nodep')

platforms = WebCL.getPlatforms()

info = []

k = (dev, str) -->
    d.getInfo(WebCL[str])

nv-ver = (M, m) -> 
   (M << 4) + m

nv-arch-cores = {
    0x10: 8 
    0x11: 8
    0x12: 8
    0x13: 8 
    0x20: 32 
    0x21: 48 
}

extensions = (dev) ->
    (k dev, \DEVICE_EXTENSIONS) / ' '

for p in platforms 

    infop         = {}
    infop.name    = p.getInfo(WebCL.PLATFORM_NAME);
    infop.version = p.getInfo(WebCL.PLATFORM_VERSION);
    infop.devices = []

    for d in p.get-devices(WebCL.DEVICE_TYPE_ALL)

        o = k(d)

        infod         = {}
        infod.name    = o \DEVICE_NAME
        infod.version = o \DEVICE_VERSION

        tt            = parseInt(o \DEVICE_TYPE)
        infod.type = 
            | tt .&. WebCL.DEVICE_TYPE_CPU => 'cpu'
            | tt .&. WebCL.DEVICE_TYPE_GPU => 'gpu'
            | tt .&. WebCL.DEVICE_TYPE_ACCELERATOR => 'acc'
            | tt .&. WebCL.DEVICE_TYPE_DEFAULT => 'def'

        infod.n-compute-units = o \DEVICE_MAX_COMPUTE_UNITS
        infod.work-item-size  = o \DEVICE_MAX_WORK_ITEM_SIZES
        infod.work-group-size = o \DEVICE_MAX_WORK_GROUP_SIZE
        infod.frequency       = o \DEVICE_MAX_CLOCK_FREQUENCY
        infod.local-mem-size  = o \DEVICE_LOCAL_MEM_SIZE
        infod.const-mem-size  = o \DEVICE_MAX_CONSTANT_BUFFER_SIZE

        cs = o \DEVICE_GLOBAL_MEM_CACHE_TYPE

        infod.cache-type = 
            | cs == WebCL.NONE  => 'no'
            | cs == WebCL.READ_ONLY_CACHE => 'ro'
            | cs == WebCL.READ_WRITE_CACHE => 'rw'

        infod.cache-size-kb = o \DEVICE_GLOBAL_MEM_CACHE_SIZE

        infod.extensions = extensions d

        if infod.extensions[\nv_device_attribute_query] 
            infod.nv = {}
            maj = o \DEVICE_COMPUTE_CAPABILITY_MAJOR_NV
            min = o \DEVICE_COMPUTE_CAPABILITY_MINOR_NV
            infod.nv.multi-procs = infod.n-compute-units 
            infod.nv.cores = 
                | nv-arch-cores[nv-ver(maj, min)]? => nv-arch-cores[nv-ver(maj, min)]
                | _ => 'NA'

        infop.devices.push(infod)
    info.push(infop)

prs = require('ansidown')

x = (s) -> 
    console.log prs(s).toString()

md = (s) ->
    prs(s).toString()

chalk = require('chalk');

spark-line = (min, max, med, unit, size, text, value) -->

    perc = (v) ->
        (v - min)/(max - min)

    value-perc  = perc value
    med-perc    = perc med

    text-size   = Math.floor(size * 0.3) + 1
    bar-size-um = Math.floor(size * 0.6 * med-perc)
    bar-size-am = Math.floor(size * 0.6 * (1 - med-perc ))



    val-size    = size - text-size - bar-size-um - bar-size-am - 1
    ttext       = _.lpad(_(text).prune(text-size), text-size)

    vbar-size = (bar-size-um + bar-size-am) * value-perc


    value-bar = ['━' for i in [1 to vbar-size] ] * ''

    
    lb-text = value-bar.slice(0,bar-size-um)
    ub-text = value-bar.slice(bar-size-um)


    lb          = _.rpad(lb-text, bar-size-um)
    ub          = _.rpad(ub-text, bar-size-am)

    ttval       = _.lpad(_(_.number-format(value)+unit).prune(val-size), val-size)





    brightCyan= (s) ->
        chalk.bgCyan.bold(s)

    darkCyan= (s) ->
        chalk.bgCyan(s)

    darkGray = (s) ->
        chalk.inverse.bold(s)


    sl = chalk.bold(ttext) + " " + brightCyan(lb) + darkGray(ub) + " " + chalk.yellow(ttval)
    return sl






x '# OpenCL device query - (c) 2014 - Vittorio Zaccaria' 
for p in info 
    x "**platform information**: #{p.name}, **platform version**: #{p.version}"
    console.log spark-line(0, 10, 7, 'Kb', 35, "cache", 8) 


