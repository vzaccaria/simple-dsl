{-# LANGUAGE QuasiQuotes #-}
module Main where
{- Copyright (c) 2011 Luis Cabellos -}
import Control.Parallel.OpenCL
import Foreign( castPtr, nullPtr, sizeOf )
import Foreign.C.Types( CFloat )
import Foreign.Marshal.Array( newArray, peekArray )
import Language.C.Quote.OpenCL
import Text.PrettyPrint.Mainland


programSource :: String
programSource = show  [cunit|
__kernel void duparray(__global float *in, __global float *out)
{
  int id = get_global_id(0);
  out[id] = 2*in[id];
}
|]

main :: IO ()
main = do

  putStrLn "Compiling:"
  putStrLn programSource

  -- Initialize OpenCL
  (platform:_) <- clGetPlatformIDs
  (dev:_) <- clGetDeviceIDs platform CL_DEVICE_TYPE_ALL
  context <- clCreateContext [CL_CONTEXT_PLATFORM platform] [dev] print
  q <- clCreateCommandQueue context dev []
  
  -- Initialize Kernel
  program <- clCreateProgramWithSource context programSource
  clBuildProgram program [dev] ""
  kernel <- clCreateKernel program "duparray"
  
  -- Initialize parameters
  let original = [0 .. 20] :: [CFloat]
      elemSize = sizeOf (0 :: CFloat)
      vecSize = elemSize * length original
  putStrLn $ "Original array = " ++ show original
  input  <- newArray original

  mem_in <- clCreateBuffer context [CL_MEM_READ_ONLY
                                   ,CL_MEM_COPY_HOST_PTR]
                                   (vecSize, castPtr input)  
  mem_out <- clCreateBuffer context [CL_MEM_WRITE_ONLY]
                                    (vecSize, nullPtr)

  clSetKernelArgSto kernel 0 mem_in
  clSetKernelArgSto kernel 1 mem_out
  
  -- Execute Kernel
  eventExec <- clEnqueueNDRangeKernel q kernel [length original] [1] []
  
  -- Get Result
  eventRead <- clEnqueueReadBuffer q mem_out True 0 vecSize (castPtr input)
                                                            [eventExec]
  
  result <- peekArray (length original) input
  putStrLn $ "Result array = " ++ show result

  return ()