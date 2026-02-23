@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Child Interface View for the Items'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
  serviceQuality: #X,
  sizeCategory: #S,
  dataClass: #MIXED
}
define view entity ZMK_SALE_O
  as select from zmk_deepak_t
  association to parent ZMK_SALE_I as _salesHeader 
    on $projection.SalesDocument = _salesHeader.SalesDocument
{
  key salesdocument         as SalesDocument,
  key salesitemnumber       as SalesItemnumber,
      material              as Material,
      plant                 as Plant,
      @Semantics.quantity.unitOfMeasure: 'Quantityunits'
      quantity              as Quantity,
      @Consumption.valueHelpDefinition: [{ entity: { name: 'I_UnitOfMeasure', element: 'UnitOfMeasure' } }]
      quantityunits         as Quantityunits,
      @Semantics.amount.currencyCode: 'Currency'
      netprice  as NetPrice,
       @Consumption.valueHelpDefinition: [{ entity: { name: 'I_Currency', element: 'Currency' } }]
      currency  as Currency,
      @Semantics.user.createdBy: true
      local_created_by      as LocalCreatedBy,
      @Semantics.systemDateTime.createdAt: true
      local_created_at      as LocalCreatedAt,
      @Semantics.user.lastChangedBy: true
      local_last_changed_by as LocalLastChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at as LocalLastChangedAt,
      
      /* Associations */
      _salesHeader
}
