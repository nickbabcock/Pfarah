namespace Pfarah

open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Reflection
open Pfarah
open System.IO

[<TypeProvider>]
type public ParaProvider (config : TypeProviderConfig) as this =
  inherit TypeProviderForNamespaces ()

  let ns = "Pfarah"
  let asm = Assembly.GetExecutingAssembly()
  let paraProvTy = ProvidedTypeDefinition(asm, ns, "ParaProvider", Some typeof<obj>)
  
  let buildTypes (typeName:string) (args:obj[]) =
    let sample = args.[0] :?> string
    let tpType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>)
    let orig = ProvidedProperty("ParaValue", typeof<string>, GetterCode = (fun args -> <@@ sample @@>))
    tpType.AddMember(orig)

    [ // Generate static Parse method
      let args = [ ProvidedParameter("text", typeof<string>) ]
      let m = ProvidedMethod("Parse", args, typeof<string>, IsStaticMethod = true)
      m.InvokeCode <- fun (text) -> <@@ () @@> 
      m.AddXmlDoc <| sprintf "Parses the specified Clausewitz generated string"
      yield m
      
      // Generate static Load stream method
      let args = [ ProvidedParameter("stream", typeof<Stream>) ]
      let m = ProvidedMethod("Load", args, typeof<string>, IsStaticMethod = true)
      m.InvokeCode <- fun (stream) -> <@@ () @@>
      m.AddXmlDoc <| sprintf "Loads the Clausewitz data from the specified stream"
      yield m ] |> tpType.AddMembers 

    tpType.AddMember(ProvidedProperty("Sample", typeof<ParaValue>, GetterCode = (fun args -> <@@ ParaValue.Parse sample @@>)))
    tpType.AddMember(ProvidedConstructor([], InvokeCode = (fun args -> <@@ () @@>)))
    tpType

  let parameters = [ProvidedStaticParameter("Sample", typeof<string>)]
    
  do paraProvTy.DefineStaticParameters(parameters, buildTypes)
  do this.AddNamespace(ns, [paraProvTy])

[<TypeProviderAssembly>]
do()