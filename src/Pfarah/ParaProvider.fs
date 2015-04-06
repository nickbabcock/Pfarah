namespace Pfarah

open ProviderImplementation
open ProviderImplementation.ProvidedTypes
open Microsoft.FSharp.Core.CompilerServices
open System.Reflection
open Pfarah

[<TypeProvider>]
type public ParaProvider (config : TypeProviderConfig) as this =
  inherit TypeProviderForNamespaces ()

  let ns = "Pfarah.ParaProvider.Provided"
  let asm = Assembly.GetExecutingAssembly()
  let paraProvTy = ProvidedTypeDefinition(asm, ns, "ParaProvider", Some typeof<obj>)
  
  let buildTypes (typeName:string) (args:obj[]) =
    let tpType = ProvidedTypeDefinition(asm, ns, typeName, Some typeof<obj>)
    let sample = args.[0] :?> string
    let document = ParaValue.Parse sample
    let orig = ProvidedProperty("ParaValue", typeof<ParaValue>, GetterCode = (fun args -> <@@ document @@>))
    tpType.AddMember(orig)

    // Generate static Parse method
//    let args = [ ProvidedParameter("text", typeof<string>) ]
//    let m = ProvidedMethod("Parse", args, typeof<string>, IsStaticMethod = true)
//    m.InvokeCode <- fun (Singleton text) -> 
//        <@ new StringReader(%%text) :> TextReader @>
//        |> spec.CreateFromTextReader 
//    m.AddXmlDoc <| sprintf "Parses the specified Clausewitz generated string"
//    tpType.AddMember m

    tpType

  let parseTextAtDesignTime resource =
    ParaValue.Parse resource

  let parameters = [ProvidedStaticParameter("Sample", typeof<string>)]
    
  do paraProvTy.DefineStaticParameters(parameters, buildTypes)
  do this.AddNamespace(ns, [paraProvTy])