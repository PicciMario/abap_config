@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'zmp_c_param_M'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}

@UI: {
  headerInfo: { typeName: 'Parameter', typeNamePlural: 'Parameters', title: { type: #STANDARD, value: 'config_key' } }
}

define view entity ZMP_C_PARAM_M
  as projection on ZMP_I_PARAM_M
{

      @UI.facet: [ { id:              'Param',
                     purpose:         #STANDARD,
                     type:            #IDENTIFICATION_REFERENCE,
                     label:           'Parameter data',
                     position:        10 }]

      @UI: {
//        lineItem:       [{ position: 10, importance: #HIGH }],
        identification: [{ position: 10, label: 'Function' }]
      }
  key function,
      @UI: {
        lineItem:       [ { position: 20, importance: #HIGH } ],
        identification: [ { position: 20, label: 'Config key' } ]
      }
  key config_key,
      @UI: {
        lineItem:       [ { position: 30, importance: #HIGH } ],
        identification: [ { position: 30, label: 'Company' } ]
      }
  key company,
      @UI: {
        lineItem:       [ { position: 40, importance: #HIGH } ],
        identification: [ { position: 40, label: 'Plant' } ]
      }
  key plant,

      @UI: {
        lineItem:       [ { position: 50, importance: #HIGH } ],
        identification: [ { position: 50, label: 'Config value' } ]
      }
      config_value,
      
      @UI.hidden: true
      last_changed_at,

      /* Associations */
      _Function : redirected to parent ZMP_C_FUNCTIONS_M
}
