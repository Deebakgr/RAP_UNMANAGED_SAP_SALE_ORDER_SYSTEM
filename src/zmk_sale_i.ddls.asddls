@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Root Interface View for the Header'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZMK_SALE_I 
  as select from zmk_deebak_t as salesHeader
  composition [0..*] of ZMK_SALE_O as _salesitem
{
  key salesdocument         as SalesDocument,
      salesdocumenttype     as SalesDocumentType,
      orderreason           as OrderReason,
      salesorganization     as SalesOrganization,
      distributionchannel   as DistributionChannel,
      division              as Division,
      salesoffice           as SalesOffice,
      salesgroup            as SalesGroup,
      
      @Semantics.amount.currencyCode: 'Currency'
      netprice              as NetPrice,
      
      @Consumption.valueHelpDefinition: [{ entity: { name: 'I_Currency', element: 'Currency' } }]
      currency              as Currency,

      // --------------------------------------------------
      // 1. EXISTING INVOICE GENERATION (Output)
      // --------------------------------------------------
      @Semantics.largeObject: { mimeType: 'MimeType', fileName: 'FileName', contentDispositionPreference: #INLINE }
      attachment            as Attachment,
      @Semantics.mimeType: true
      mimetype              as MimeType,
      filename              as FileName,

      // --------------------------------------------------
      // 2. NEW EXCEL UPLOAD (Input)
      // --------------------------------------------------
      @Semantics.largeObject: { 
        mimeType: 'ExcelMimeType', 
        fileName: 'ExcelFileName', 
        contentDispositionPreference: #INLINE 
      }
      excel_attachment      as ExcelAttachment,

      @Semantics.mimeType: true
      excel_mimetype        as ExcelMimeType,

      excel_filename        as ExcelFileName,

      // --------------------------------------------------
      // Audit Fields
      // --------------------------------------------------
      @Semantics.user.createdBy: true
      local_created_by      as LocalCreatedBy,
      @Semantics.systemDateTime.createdAt: true
      local_created_at      as LocalCreatedAt,
      @Semantics.user.lastChangedBy: true
      local_last_changed_by as LocalLastChangedBy,
      @Semantics.systemDateTime.localInstanceLastChangedAt: true
      local_last_changed_at as LocalLastChangedAt,
      @Semantics.systemDateTime.lastChangedAt: true
      last_changed_at       as LastChangedAt,
      
      /* Associations */
      _salesitem
}
