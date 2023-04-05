@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZMP_I_FUNCTIONS_M'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZMP_I_PARAM_M
  as select from zmp_param
  association to parent ZMP_I_FUNCTIONS_M as _Function on $projection.function = _Function.function
{
  key function,
  key config_key,
  key company,
  key plant,
      config_value,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      last_changed_at,

      _Function
}
