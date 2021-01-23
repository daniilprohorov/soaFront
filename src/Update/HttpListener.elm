module Update.HttpListener exposing (..)

import DataField exposing (DataField(..))
import Debug exposing (toString)
import HttpActions exposing (httpProducts)
import Organizations exposing (getOrganizations)
import Products exposing (getProduct, getProducts)
import Types exposing (HttpMsg(..), Model(..), Operation(..), defMain)
import Utils exposing (errorToString)

httpUpdate httpMsg = case httpMsg of
    HttpGetProducts result filters filterApply elemsperpage page ->
          let
              productsList = case result of
                  Ok fullText -> getProducts fullText
                  Err e -> Err <| toString e
              products = case productsList of
                  Ok prds -> Just <| Prds prds
                  Err e -> Nothing
          in
              (ProductsPage (Main Nothing filters filterApply elemsperpage page) products, Cmd.none)

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

    HttpAddProduct result -> (ProductsPage defMain Nothing, httpProducts Nothing Nothing False 20 1 )

    HttpDeleteProduct result -> (ProductsPage defMain Nothing, httpProducts Nothing Nothing False 20 1 )

    HttpShowProduct result -> case result of
        Ok res ->
          let
              productsList = case result of
                  Ok fullText -> getProduct fullText
                  Err e -> Err <| toString e
              product = case productsList of
                  Ok prd -> Just <| Prd prd
                  Err e -> Nothing
          in
             (ProductsPage (ShowById 0 True Nothing Nothing) product, Cmd.none )
        Err e -> (ProductsPage (ShowById 0 True (Just (e |> errorToString)) Nothing) Nothing, Cmd.none )

    HttpEditProduct result -> (ProductsPage defMain Nothing, httpProducts Nothing Nothing False 20 1 )
