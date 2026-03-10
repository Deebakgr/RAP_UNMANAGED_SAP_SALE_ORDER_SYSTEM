# RAP Unmanaged SAP Sale Order System

## Project Overview

The **RAP Unmanaged SAP Sale Order System** is developed using the **ABAP RESTful Application Programming Model (RAP)** with an **Unmanaged Scenario**. This application demonstrates how to build a custom business application in SAP where the developer manually handles the business logic for database operations.

The system enables users to manage **sales orders**, including creating, updating, viewing, and deleting order records. Unlike the managed scenario, the unmanaged scenario provides greater flexibility by allowing developers to implement the business logic explicitly within the behavior implementation class.

---

## Objectives

The main objectives of this project are:

* To demonstrate the **RAP Unmanaged Scenario** in SAP ABAP.
* To develop a **Sales Order Management Application**.
* To manually implement CRUD operations in the behavior implementation class.
* To create a **Fiori-based user interface** using RAP services.
* To understand advanced RAP development concepts.

---

## Technologies Used

* **ABAP RESTful Application Programming Model (RAP)**
* **Core Data Services (CDS)**
* **Behavior Definition (Unmanaged Implementation)**
* **Behavior Implementation Class**
* **Service Definition**
* **Service Binding (OData V4)**
* **SAP Fiori Elements**

---

## System Architecture

The application follows the **RAP Unmanaged architecture**:

1. **Database Table**

   * Stores sales order details.

2. **Interface CDS View**

   * Represents the business object interface.

3. **Behavior Definition (Unmanaged)**

   * Defines operations but does not automatically handle them.

4. **Behavior Implementation Class**

   * Contains custom logic for Create, Update, Delete, and Read operations.

5. **Projection CDS View**

   * Used for UI consumption.

6. **Service Definition**

   * Exposes the business object.

7. **Service Binding**

   * Connects the application to SAP Fiori through OData services.

---

## Key Features

* Create new sales orders
* View existing sales orders
* Update order details
* Delete sales orders
* Custom business logic implementation
* Fiori-based UI for order management

---

## Sales Order Data Fields

| Field Name    | Description                     |
| ------------- | ------------------------------- |
| Order_ID      | Unique identifier for the order |
| Customer_Name | Name of the customer            |
| Product_Name  | Name of the product             |
| Quantity      | Number of products ordered      |
| Price         | Price per product               |
| Total_Amount  | Total order value               |

---

