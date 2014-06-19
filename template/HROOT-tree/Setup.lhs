#! /usr/bin/env runhaskell
  
> import Distribution.Simple
> import Distribution.Simple.Setup
> import Distribution.PackageDescription
> import Distribution.Simple.LocalBuildInfo
>
> import Config
>
> myconfigHook = simpleUserHooks { confHook = hookfunction } 
>
> hookfunction x y = do 
>   binfo <- confHook simpleUserHooks x y 
>   r_pbi <- config binfo
>   let pkg_descr = localPkgDescr binfo
>   
>   let newbinfo = case r_pbi of 
>                    Just pbi ->  binfo { localPkgDescr = updatePackageDescription pbi pkg_descr }
>                    Nothing -> do 
>                      let r_lib = library pkg_descr 
>                      case r_lib of
>                        Just lib ->  
>                          let binfo2 = libBuildInfo lib
>                              newlib = lib { libBuildInfo = binfo2 { cSources = [] }}  
>                          in  binfo { localPkgDescr = pkg_descr { library = Just newlib }}  
>                        Nothing -> error "some library setting is wrong." 
> --   putStrLn (show (localPkgDescr newbinfo))
>   return newbinfo
>  
>
> main = defaultMainWithHooks myconfigHook
>

