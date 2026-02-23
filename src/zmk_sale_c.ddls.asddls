
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Sales Order Header Consumption View'
@Search.searchable: true
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZMK_SALE_C
  provider contract transactional_query
  as projection on ZMK_SALE_I
{
  key SalesDocument,
      SalesDocumentType,
      OrderReason,
      SalesOrganization,
      DistributionChannel,
      Division,
      
      @Search.defaultSearchElement: true
      SalesOffice,
      
      SalesGroup,
      
      @Semantics.amount.currencyCode: 'Currency'
      NetPrice,
      
      @Consumption.valueHelpDefinition: [{ entity: { name: 'I_Currency', element: 'Currency' } }]
      @Semantics.currencyCode: true    // <--- ADDED: Helps UI identify this as the unit field
      Currency,

    // --------------------------------------------------
    // 1. EXISTING INVOICE (Output / Download)
    // --------------------------------------------------
    @Semantics.largeObject: {
      mimeType: 'MimeType',
      fileName: 'FileName',
      contentDispositionPreference: #ATTACHMENT
    }
    Attachment,
    
    @Semantics.mimeType: true
    MimeType,
    
    FileName,

    // --------------------------------------------------
    // 2. NEW EXCEL UPLOAD (Input / Upload)
    // --------------------------------------------------
    @Semantics.largeObject: {
      mimeType: 'ExcelMimeType',
      fileName: 'ExcelFileName',
      contentDispositionPreference: #ATTACHMENT
    }
    ExcelAttachment,

    @Semantics.mimeType: true
    ExcelMimeType,

    ExcelFileName,

    // --------------------------------------------------
    // Audit Fields
    // --------------------------------------------------
    LocalCreatedBy,
    LocalCreatedAt,
    LocalLastChangedBy,
    LocalLastChangedAt,
    LastChangedAt,     // <--- ADDED: Crucial for ETag / Concurrency Control
      
    /* Associations */
    _salesitem : redirected to composition child ZMK_SALE_O_C
}
