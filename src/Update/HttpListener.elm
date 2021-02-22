module Update.HttpListener exposing (..)

import DataField exposing (DataField(..))
import Debug exposing (toString)
import HttpActions exposing (httpOrganizations, httpPriceAvg, httpPriceSum, httpProducts)
import Organizations exposing (getOrganization, getOrganizations)
import Products exposing (getProduct, getProducts)
import Types exposing (HttpMsg(..), Model(..), Operation(..), defMain)
import Utils exposing (errorToString)

httpUpdate httpMsg model = case httpMsg of
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

    HttpGetOrganizations result filters filterApply elemsperpage page->
          let
              organizationsList = case result of
                  Ok fullText -> getOrganizations fullText
                  Err e -> Err <| toString e
              organizations = case organizationsList of
                  Ok orgs -> Just <| Orgs orgs
                  Err e -> Nothing
          in
              (OrganizationsPage (Main Nothing filters filterApply elemsperpage page) organizations, Cmd.none)

    HttpAddProduct result -> (ProductsPage defMain Nothing, httpProducts Nothing Nothing False 20 1 )
    HttpAddOrganization result -> (OrganizationsPage defMain Nothing, httpOrganizations Nothing Nothing False 20 1 )

    HttpDeleteProduct result -> (ProductsPage defMain Nothing, httpProducts Nothing Nothing False 20 1 )
    HttpDeleteOrganization result -> (OrganizationsPage defMain Nothing, httpOrganizations Nothing Nothing False 20 1 )

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

    HttpShowOrganization result -> case result of
        Ok res ->
          let
              organizationsList = case result of
                  Ok fullText -> getOrganization fullText
                  Err e -> Err <| toString e
              organization = case organizationsList of
                  Ok prd -> Just <| Org prd
                  Err e -> Nothing
          in
             (OrganizationsPage (ShowById 0 True Nothing Nothing) organization, Cmd.none )
        Err e -> (OrganizationsPage (ShowById 0 True (Just (e |> errorToString)) Nothing) Nothing, Cmd.none )

    HttpEditProduct result -> (ProductsPage defMain Nothing, httpProducts Nothing Nothing False 20 1 )
    HttpEditOrganization result -> (OrganizationsPage defMain Nothing, httpOrganizations Nothing Nothing False 20 1 )

    HttpPriceSum result -> case result of
        Ok res -> (MainPage (Just res) Nothing, httpPriceAvg)
        Err e -> (MainPage (Just "error") Nothing, httpPriceAvg)

    HttpPriceAvg result -> case model of
        MainPage s a -> case result of
            Ok res -> (MainPage s (Just res), Cmd.none)
            Err e -> (MainPage s (Just "error") , Cmd.none)

        ProductsPage operation maybeDataField -> (MainPage Nothing Nothing, httpPriceSum)


        OrganizationsPage operation maybeDataField -> (MainPage Nothing Nothing, httpPriceSum)

