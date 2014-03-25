
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


platform-desc = (p)->
"""
**platform information**: #{p.name}, **platform version**: #{p.version}


"""

prs = require('ansidown')

for p in info 
    console.log prs(platform-desc p)