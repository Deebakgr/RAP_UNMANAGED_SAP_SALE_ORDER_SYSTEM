@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Item Consumption View'
@Search.searchable: true
@Metadata.allowExtensions: true
define view entity ZMK_SALE_O_C
  as projection on ZMK_SALE_O
{
  key SalesDocument,
  key SalesItemnumber,
      @Search.defaultSearchElement: true
      Material,
      Plant,
      @Semantics.quantity.unitOfMeasure: 'Quantityunits'
      Quantity,
      @Consumption.valueHelpDefinition: [{ entity: { name: 'I_UnitOfMeasure', element: 'UnitOfMeasure' } }]
      Quantityunits,
      
      @Semantics.amount.currencyCode: 'Currency'
      NetPrice,
      @Consumption.valueHelpDefinition: [{ entity: { name: 'I_Currency', element: 'Currency' } }]
      @Semantics.currencyCode: true   
      Currency,

      LocalCreatedBy,
      LocalCreatedAt,
      LocalLastChangedBy,
      LocalLastChangedAt,

      /* Associations */
      _salesHeader : redirected to parent ZMK_SALE_C
}
