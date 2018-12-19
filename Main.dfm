object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 371
  ClientWidth = 657
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object btnSolve: TButton
    Left = 8
    Top = 35
    Width = 145
    Height = 25
    Caption = 'Solve'
    TabOrder = 0
    OnClick = btnSolveClick
  end
  object cbb1: TComboBox
    Left = 8
    Top = 8
    Width = 145
    Height = 21
    ItemIndex = 0
    TabOrder = 1
    Text = 'Day 1'
    Items.Strings = (
      'Day 1'
      'Day 2'
      'Day 3'
      'Day 4'
      'Day 5'
      'Day 6'
      'Day 7'
      'Day 8'
      'Day 9'
      'Day 10'
      'Day 11'
      'Day 12'
      'Day 13'
      'Day 14'
      'Day 15'
      'Day 16'
      'Day 17'
      'Day 18'
      'Day 19'
      'Day 20'
      'Day 21'
      'Day 22'
      'Day 23'
      'Day 24'
      'Day 25')
  end
end
