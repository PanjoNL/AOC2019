object Cryostasis: TCryostasis
  Left = 0
  Top = 0
  Caption = 'Cryostasis'
  ClientHeight = 616
  ClientWidth = 1110
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object pnl1: TPanel
    Left = 0
    Top = 0
    Width = 1110
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitLeft = 472
    ExplicitTop = 304
    ExplicitWidth = 185
    object btnNorth: TButton
      Left = 104
      Top = 10
      Width = 75
      Height = 25
      Caption = 'North'
      TabOrder = 0
      OnClick = btnNorthClick
    end
    object btnEast: TButton
      Left = 185
      Top = 10
      Width = 75
      Height = 25
      Caption = 'East'
      TabOrder = 1
      OnClick = btnEastClick
    end
    object btnSouth: TButton
      Left = 266
      Top = 10
      Width = 75
      Height = 25
      Caption = 'South'
      TabOrder = 2
      OnClick = btnSouthClick
    end
    object btnWest: TButton
      Left = 344
      Top = 10
      Width = 75
      Height = 25
      Caption = 'West'
      TabOrder = 3
      OnClick = btnWestClick
    end
    object btnInventory: TButton
      Left = 425
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Inventory'
      TabOrder = 4
      OnClick = btnInventoryClick
    end
    object btnTake: TButton
      Left = 506
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Take'
      TabOrder = 5
      OnClick = btnTakeClick
    end
    object btnDrop: TButton
      Left = 587
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Drop'
      TabOrder = 6
      OnClick = btnDropClick
    end
    object edtItem: TEdit
      Left = 668
      Top = 14
      Width = 121
      Height = 21
      TabOrder = 7
    end
    object btnReset: TButton
      Left = 23
      Top = 11
      Width = 75
      Height = 24
      Caption = 'Reset'
      TabOrder = 8
      OnClick = btnResetClick
    end
  end
  object pnl2: TPanel
    Left = 0
    Top = 41
    Width = 1110
    Height = 575
    Align = alClient
    TabOrder = 1
    ExplicitLeft = 440
    ExplicitTop = 24
    ExplicitWidth = 185
    ExplicitHeight = 41
    object mmo1: TMemo
      Left = 1
      Top = 1
      Width = 1108
      Height = 573
      Align = alClient
      TabOrder = 0
      ExplicitLeft = 744
      ExplicitTop = 368
      ExplicitWidth = 185
      ExplicitHeight = 89
    end
  end
end
