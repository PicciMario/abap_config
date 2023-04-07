@EndUserText.label: 'ZMP_A_SIMULA'
define abstract entity ZMP_A_SIMULA
{
  @EndUserText.label: 'Config key'
  config_key : zmp_de_confkey;
  @EndUserText.label: 'Company'
  company    : abap.char(5);
  @EndUserText.label: 'Plant'
  plant      : abap.char(5);
}
