#!/usr/bin/env node
(function(){
  var _, moment, fs, color, os, shelljs, table, name, description, author, year, src, otm, cwd, setupTemporaryDirectory, removeTemporaryDirectory, usageString, optimist, argv, command, WebCL, platforms, info, k, nvVer, nvArchCores, nvMinCores, nvMaxCores, nvMinSm, nvMaxSm, extensions, i$, len$, p, infop, j$, ref$, len1$, d, o, infod, tt, cs, maj, min, prs, chalk, sparkLine, indent, indentSize, paragraphTitle, bop, eop, O, md, bytes, computeUnits, wgsize, lmsize, cmsize, csize, corenum, totcore, split$ = ''.split;
  _ = require('underscore');
  _.str = require('underscore.string');
  moment = require('moment');
  fs = require('fs');
  color = require('ansi-color').set;
  os = require('os');
  shelljs = require('shelljs');
  table = require('ansi-color-table');
  _.mixin(_.str.exports());
  _.str.include('Underscore.string', 'string');
  name = "Device Query";
  description = "OpenCL device query for dummies";
  author = "Vittorio Zaccaria";
  year = "2013";
  src = __dirname;
  otm = os.tmpdir != null ? os.tmpdir() : "/var/tmp";
  cwd = process.cwd();
  setupTemporaryDirectory = function(){
    var name, dire;
    name = "tmp_" + moment().format('HHmmss') + "_tmp";
    dire = otm + "/" + name;
    shelljs.mkdir('-p', dire);
    return dire;
  };
  removeTemporaryDirectory = function(dir){
    return shelljs.rm('-rf', dir);
  };
  usageString = "\n" + color(name, 'bold') + ". " + description + "\n(c) " + author + ", " + year + "\n\nUsage: " + name + " [--option=V | -o V] ";
  optimist = require('optimist');
  argv = optimist.usage(usageString, {
    help: {
      alias: 'h',
      description: 'this help',
      'default': false
    }
  }).boolean('h').argv;
  if (argv.help) {
    optimist.showHelp();
    return;
  }
  command = argv._;
  WebCL = require('webcl-nodep');
  platforms = WebCL.getPlatforms();
  info = [];
  k = curry$(function(dev, str){
    return d.getInfo(WebCL[str]);
  });
  nvVer = function(M, m){
    return function(){
      return M(4 .apply(this, arguments));
    } + m;
  };
  nvArchCores = {
    0x10: 8,
    0x11: 8,
    0x12: 8,
    0x13: 8,
    0x20: 32,
    0x21: 48,
    0x30: 192,
    0x35: 192,
    0x50: 128
  };
  nvMinCores = 8;
  nvMaxCores = 192;
  nvMinSm = 1;
  nvMaxSm = 30;
  extensions = function(dev){
    return split$.call(k(dev, 'DEVICE_EXTENSIONS'), ' ');
  };
  for (i$ = 0, len$ = platforms.length; i$ < len$; ++i$) {
    p = platforms[i$];
    infop = {};
    infop.name = p.getInfo(WebCL.PLATFORM_NAME);
    infop.version = p.getInfo(WebCL.PLATFORM_VERSION);
    infop.devices = [];
    for (j$ = 0, len1$ = (ref$ = p.getDevices(WebCL.DEVICE_TYPE_ALL)).length; j$ < len1$; ++j$) {
      d = ref$[j$];
      o = k(d);
      infod = {};
      infod.name = o('DEVICE_NAME');
      infod.version = o('DEVICE_VERSION');
      tt = parseInt(o('DEVICE_TYPE'));
      infod.type = (fn$());
      infod.nComputeUnits = o('DEVICE_MAX_COMPUTE_UNITS');
      infod.workItemSize = o('DEVICE_MAX_WORK_ITEM_SIZES');
      infod.workGroupSize = o('DEVICE_MAX_WORK_GROUP_SIZE');
      infod.frequency = o('DEVICE_MAX_CLOCK_FREQUENCY');
      infod.localMemSize = o('DEVICE_LOCAL_MEM_SIZE');
      infod.constMemSize = o('DEVICE_MAX_CONSTANT_BUFFER_SIZE');
      cs = o('DEVICE_GLOBAL_MEM_CACHE_TYPE');
      infod.cacheType = (fn1$());
      infod.cacheSizeKb = o('DEVICE_GLOBAL_MEM_CACHE_SIZE');
      infod.extensions = extensions(d);
      if (infod.extensions['nv_device_attribute_query']) {
        infod.nv = {};
        maj = o('DEVICE_COMPUTE_CAPABILITY_MAJOR_NV');
        min = o('DEVICE_COMPUTE_CAPABILITY_MINOR_NV');
        infod.nv.multiProcs = infod.nComputeUnits;
        infod.nv.cores = (fn2$());
      }
      infop.devices.push(infod);
    }
    info.push(infop);
  }
  prs = require('ansidown');
  chalk = require('chalk');
  sparkLine = curry$(function(min, max, med, unit, size, text, value){
    var perc, valuePerc, medPerc, textSize, barSizeUm, barSizeAm, valSize, ttext, vbarSize, valueBar, i, lbText, ubText, lb, ub, ttval, brightCyan, darkCyan, darkGray, sl;
    perc = function(v){
      return (v - min) / (max - min);
    };
    valuePerc = perc(value);
    medPerc = perc(med);
    textSize = Math.floor(size * 0.2) + 1;
    barSizeUm = Math.floor(size * 0.6 * medPerc);
    barSizeAm = Math.floor(size * 0.6 * (1 - medPerc));
    valSize = size - textSize - barSizeUm - barSizeAm - 1;
    ttext = _.lpad(_(text).prune(textSize), textSize);
    vbarSize = (barSizeUm + barSizeAm) * valuePerc;
    valueBar = (function(){
      var i$, ref$, len$, results$ = [];
      for (i$ = 0, len$ = (ref$ = (fn$())).length; i$ < len$; ++i$) {
        i = ref$[i$];
        results$.push('━');
      }
      return results$;
      function fn$(){
        var i$, to$, results$ = [];
        for (i$ = 1, to$ = vbarSize; i$ <= to$; ++i$) {
          results$.push(i$);
        }
        return results$;
      }
    }()).join('');
    lbText = valueBar.slice(0, barSizeUm);
    ubText = valueBar.slice(barSizeUm);
    lb = _.rpad(lbText, barSizeUm);
    ub = _.rpad(ubText, barSizeAm);
    ttval = _.rpad(_(_.numberFormat(value) + unit).prune(valSize), valSize);
    brightCyan = function(s){
      return chalk.bgCyan.bold(s);
    };
    darkCyan = function(s){
      return chalk.bgCyan(s);
    };
    darkGray = function(s){
      return chalk.inverse.bold(s);
    };
    sl = chalk.bold(ttext) + " " + brightCyan(lb) + darkGray(ub) + " " + chalk.yellow(ttval);
    return sl;
  });
  indent = 0;
  indentSize = 9;
  paragraphTitle = '';
  bop = function(s){
    indent = indentSize;
    return paragraphTitle = _.lpad(_(s).prune(indentSize), indentSize);
  };
  eop = function(){
    var indent;
    return indent = 0;
  };
  o = function(s){
    if (indent > 0) {
      console.log(paragraphTitle, s);
      return paragraphTitle = _.lpad(_('').prune(indentSize), indentSize);
    } else {
      return console.log(s);
    }
  };
  O = function(s){
    o(s);
    return console.log("");
  };
  md = function(s){
    return prs(s).toString();
  };
  bytes = require('bytes');
  o(md('# OpenCL device query - (c) 2014 - Vittorio Zaccaria'));
  for (i$ = 0, len$ = info.length; i$ < len$; ++i$) {
    p = info[i$];
    o(md("**platform information**: " + p.name + ", **platform version**: " + p.version));
    for (j$ = 0, len1$ = (ref$ = p.devices).length; j$ < len1$; ++j$) {
      d = ref$[j$];
      computeUnits = sparkLine(1, nvMaxSm, 15, '', 35, "cunits", d.nComputeUnits);
      wgsize = sparkLine(1, 2048, 512, '', 35, "wg-size", d.workGroupSize);
      lmsize = sparkLine(1, 64, 32, 'KB', 35, "localmem", d.localMemSize / bytes('1kb'));
      cmsize = sparkLine(1, 64, 32, 'KB', 35, "constmem", d.constMemSize / bytes('1kb'));
      csize = sparkLine(1, 128, 32, 'KB', 35, "cache", d.cacheSizeKb);
      bop(d.type + ":");
      o(md("**device-name**: " + d.name + ", **frequency**: " + d.frequency));
      o(md("--"));
      if (d.nv == null) {
        d.nv = {
          cores: 1
        };
      }
      if (d.nv != null) {
        corenum = sparkLine(1, nvMaxCores, 15, '', 35, "cores/cu", d.nv.cores);
        totcore = sparkLine(1 * nvMinSm, nvMaxCores * nvMaxSm, 1000, '', 35, "cores", d.nv.cores * d.nComputeUnits);
        O(corenum + totcore);
      }
      O(computeUnits + wgsize);
      O(lmsize + cmsize);
      O(csize + md("read/write: " + d.cacheType));
      eop;
    }
  }
  function curry$(f, bound){
    var context,
    _curry = function(args) {
      return f.length > 1 ? function(){
        var params = args ? args.concat() : [];
        context = bound ? context || this : this;
        return params.push.apply(params, arguments) <
            f.length && arguments.length ?
          _curry.call(context, params) : f.apply(context, params);
      } : f;
    };
    return _curry();
  }
  function fn$(){
    switch (false) {
    case !(tt & WebCL.DEVICE_TYPE_CPU):
      return 'cpu';
    case !(tt & WebCL.DEVICE_TYPE_GPU):
      return 'gpu';
    case !(tt & WebCL.DEVICE_TYPE_ACCELERATOR):
      return 'acc';
    case !(tt & WebCL.DEVICE_TYPE_DEFAULT):
      return 'def';
    }
  }
  function fn1$(){
    switch (false) {
    case cs !== WebCL.NONE:
      return 'no';
    case cs !== WebCL.READ_ONLY_CACHE:
      return 'ro';
    case cs !== WebCL.READ_WRITE_CACHE:
      return 'rw';
    }
  }
  function fn2$(){
    switch (false) {
    case nvArchCores[nvVer(maj, min)] == null:
      return nvArchCores[nvVer(maj, min)];
    default:
      return 'NA';
    }
  }
}).call(this);
