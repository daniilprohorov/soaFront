module Update.HttpListener exposing (..)

import DataField exposing (DataField(..))
import Debug exposing (toString)
import HttpActions exposing (httpProducts)
import Organizations exposing (getOrganizations)
import Products exposing (getProducts)
import Types exposing (HttpMsg(..), Model(..), Operation(..), defMain)

httpUpdate httpMsg = case httpMsg of
    HttpGetProducts result ->
          let
              productsList = case result of
                  Ok fullText -> getProducts fullText
                  Err e -> Err <| toString e
              products = case productsList of
                  Ok prds -> Just <| Prds prds
                  Err e -> Nothing
          in
              (ProductsPage defMain products, Cmd.none)

    HttpGetOrganizations result ->
          let
              organizationsList = case result of
                  Ok fullText -> getOrganizations fullText
                  Err e -> Err <| toString e

              organizations = case organizationsList of
                  Ok orgs -> Just <| Orgs orgs
                  Err e -> Nothing
          in
              (OrganizationsPage defMain organizations, Cmd.none)

    HttpAddProduct result -> (ProductsPage defMain Nothing, httpProducts Nothing Nothing 20 1 )

    HttpDeleteProduct result -> (ProductsPage defMain Nothing, httpProducts Nothing Nothing 20 1 )

    HttpEditProduct result -> (ProductsPage defMain Nothing, httpProducts Nothing Nothing 20 1 )
