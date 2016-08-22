object AboutDNSEABridgeForm: TAboutDNSEABridgeForm
  Left = 0
  Top = 0
  Caption = 'About De Novo Software External Application Bridge'
  ClientHeight = 451
  ClientWidth = 666
  Color = clBtnFace
  Constraints.MinHeight = 200
  Constraints.MinWidth = 200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object memGPLInformation: TMemo
    Left = 0
    Top = 0
    Width = 666
    Height = 451
    Align = alClient
    Lines.Strings = (
      
        'The De Novo Software External Application Bridge accepts data vi' +
        'a TCP/IP and converts it into a format required for the opaR '
      'library.'
      ''
      'Copyright (C) 2016 De Novo Software'
      ''
      
        'This program is free software: except for portions that are deri' +
        'ved from opaR, which are licensed under the GNU Affero General '
      
        'Public License, you can redistribute it and/or modify it under t' +
        'he terms of the GNU General Public License as published by the F' +
        'ree '
      
        'Software Foundation, either version 3 of the License, or (at you' +
        'r option) any later version.'
      ''
      
        'This program is distributed in the hope that it will be useful, ' +
        'but WITHOUT ANY WARRANTY; without even the implied warranty of '
      
        'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GN' +
        'U General Public License for more details.'
      ''
      
        'You should have received a copy of the GNU General Public Licens' +
        'e along with this program.  If not, see '
      '<http://www.gnu.org/licenses/>.'
      ''
      
        'The source code for the De Novo Software External Application Br' +
        'idge can be found at '
      '< https://github.com/denovosoftware/dnseabridge>.'
      ''
      'This software includes a modified version of opaR'
      ''
      
        'opaR is Copyright (C) 2015-2016 Sigma Sciences Ltd.; originator ' +
        'Robert L S Devine'
      ''
      'Modifications Copyright 2016 by De Novo Software'
      ''
      
        'The opaR portions of this program are free software: you can red' +
        'istribute it and/or modify it under the terms of the GNU Affero '
      
        'General Public License as published by the Free Software Foundat' +
        'ion, either version 3 of the License, or (at your option) any la' +
        'ter '
      'version.   '
      ''
      
        'This program is distributed in the hope that it will be useful, ' +
        'but WITHOUT ANY WARRANTY; without even the implied warranty of '
      
        'MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GN' +
        'U Affero General Public License for more details.'
      ''
      
        'You should have received a copy of the GNU Affero General Public' +
        ' License along with this program.  If not, see '
      '<http://www.gnu.org/licenses/>.')
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
    ExplicitHeight = 436
  end
end
