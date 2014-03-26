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
    0x30: 192
    0x35: 192
    0x50: 128
}

nv-min-cores = 8
nv-max-cores = 192

nv-min-sm = 1
nv-max-sm = 30

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


chalk = require('chalk');

spark-line = (min, max, med, unit, size, text, value) -->

    perc = (v) ->
        (v - min)/(max - min)

    value-perc  = perc value
    med-perc    = perc med

    text-size   = Math.floor(size * 0.2) + 1
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

    ttval       = _.rpad(_(_.number-format(value)+unit).prune(val-size), val-size)


    brightCyan= (s) ->
        chalk.black.bgCyan.bold(s)

    darkGray = (s) ->
        chalk.inverse.bold(s)


    sl = chalk.bold(ttext) + " " + brightCyan(lb) + darkGray(ub) + " " + chalk.yellow(ttval)
    return sl



indent          = 0
indent-size     = 9
paragraph-title = ''

bop = (s) ->
    indent := indent-size
    paragraph-title := _.lpad(_(s).prune(indent-size), indent-size)

eop = ->
    indent = 0


o = (s) ->
    if indent>0
        console.log paragraph-title, s
        paragraph-title := _.lpad(_('').prune(indent-size), indent-size)
    else 
        console.log s

O = (s) ->
    o(s)
    console.log ""


md = (s) ->
    prs(s).toString()

bytes = require('bytes')

o md '# OpenCL device query - (c) 2014 - Vittorio Zaccaria' 
for p in info 
    o md "**platform information**: #{p.name}, **platform version**: #{p.version}"
    for d in p.devices

        compute-units = spark-line(1 , nv-max-sm , 15  , ''   , 35 , "cunits"   , d.n-compute-units)
        wgsize        = spark-line(1 , 2048      , 512 , ''   , 35 , "wg-size"  , d.work-group-size)
        lmsize        = spark-line(1 , 64        , 32  , 'KB' , 35 , "localmem" , (d.local-mem-size/bytes('1kb')))
        cmsize        = spark-line(1 , 64        , 32  , 'KB' , 35 , "constmem" , (d.const-mem-size/bytes('1kb')))
        csize         = spark-line(1 , 128       , 32  , 'KB' , 35 , "cache"    , (d.cache-size-kb))
        # cmsize      = spark-line(1 , 64        , 32  , ''   , 35 , "KB"       , d.const-mem-size)

        bop "#{d.type}:"

        o  md "**device-name**: #{d.name}, **frequency**: #{d.frequency}"
        o  md "--"


        if not d.nv?
            d.nv = { cores: 1 }

        if d.nv?
            corenum = spark-line(1           , nv-max-cores             , 15    , '' , 35 , "cores/cu" , (d.nv.cores))
            totcore = spark-line(1*nv-min-sm , nv-max-cores * nv-max-sm , 1000 , '' , 35 , "cores"    , (d.nv.cores * d.n-compute-units))
            O corenum + totcore            

        O  compute-units + wgsize 
        O  lmsize + cmsize
        O  csize + md("read/write: #{d.cache-type}") 
        eop


