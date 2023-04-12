@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZMP_C_FUNCTIONS_M'
@Search.searchable: true

@UI: {
  headerInfo: {
    typeName: 'Function',
    typeNamePlural: 'Functions',
    title: { type: #STANDARD, value: 'function' },
    description: { value: 'description' }
  }  
}

define root view entity ZMP_C_FUNCTIONS_M
  provider contract transactional_query
  as projection on ZMP_I_FUNCTIONS_M
{

    @UI.facet: [
    {
      label: 'General Information',
      id: 'GeneralInfo',
      type: #COLLECTION,
      position: 10
    },
        {
            id:              'Function',
            purpose:         #STANDARD,
            type:            #IDENTIFICATION_REFERENCE,
            label:           'Function data',
            position:        10
        },        
        { 
            id:              'Parameter',
            purpose:         #STANDARD,
            type:            #LINEITEM_REFERENCE,
            label:           'Configuration keys in function',
            position:        20,
            targetElement:   '_Param'
        }
    ]

      @UI: {
        lineItem:       [ { position: 10, importance: #HIGH } ],
        identification: [ { position: 10, label: 'Function' },
                          { type: #FOR_ACTION,  dataAction: 'Simula' , label: 'Simula' , position: 10 } 
                        ]
      }
      @Search.defaultSearchElement: true
  key function,

      @UI: {
        lineItem:       [ { position: 15, importance: #HIGH } ],
        identification: [ { position: 15, label: 'Parent function' } ]
      }
      parent,

      @UI: {
        lineItem:       [ { position: 20, importance: #HIGH } ],
        identification: [ { position: 20, label: 'Description' } ]
      }
      description,
      
      @UI: {
        identification: [ { position: 30, label: 'Note' } ],
        multiLineText: true
      }      
      note,

      @UI.hidden: true
      last_changed_at,

      /* Associations */
      _Param : redirected to composition child ZMP_C_PARAM_M
}
