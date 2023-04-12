@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'ZMP_I_FUNCTIONS_M'
define root view entity ZMP_I_FUNCTIONS_M
  as select from zmp_functions
  composition [0..*] of ZMP_I_PARAM_M as _Param
{

  key function,
      parent,
      description,
      note,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      last_changed_at,
      _Param

}
