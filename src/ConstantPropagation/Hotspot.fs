namespace Hotspot

type Hotspot = 
    val Language : string
    val Class : string
    val Method : string
    val QueryPosition : int 
    val ReturnType : string

    new (language : string, className : string, methodName : string, position, returnType : string) = {
        Language = language.ToLowerInvariant() 
        Class = className.ToLowerInvariant()
        Method = methodName.ToLowerInvariant()
        QueryPosition = position
        ReturnType = returnType.ToLowerInvariant()
    }
    (*new (language, full : string array, position, returnType) = 
        new Hotspot(language, full.[0], full.[1], position, returnType)*)