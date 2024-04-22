//
//  NextActionParamTuple.swift
//  TUILiveKit
//
//  Created by aby on 2024/3/15.
//

typealias NextActionParamTuple<T> = (param: T, nextActions: [Action])
typealias NextActionTemplateParamTuple<T, Payload> = (param: T, nextActionTemplates: [ActionTemplate<Payload>])


func generateActionParamTuple<T>(param: T, actions:[Action]) -> NextActionParamTuple<T> {
    return (param: param, nextActions: actions)
}

func generateActionTemplateParamTuple<T, Payload>(param: T, actions:[ActionTemplate<Payload>]) -> NextActionTemplateParamTuple<T, Payload> {
    return (param: param, nextActionTemplates: actions)
}
