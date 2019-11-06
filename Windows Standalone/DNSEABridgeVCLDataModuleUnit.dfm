object DNSEABridgeVCLDataModule: TDNSEABridgeVCLDataModule
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Height = 246
  Width = 355
  object DNSEABridgeActionList: TActionList
    Left = 248
    Top = 16
    object acRunCluster: TAction
      Caption = 'acRunCluster'
      OnExecute = acRunClusterExecute
    end
    object acNewParam: TAction
      Caption = 'acNewParam'
      OnExecute = acNewParamExecute
    end
    object acCloseDNSEABridge: TAction
      Caption = 'acCloseDNSEABridge'
      OnExecute = acCloseDNSEABridgeExecute
    end
    object acGetExceptionsText: TAction
      Caption = 'acGetExceptionsText'
      OnExecute = acGetExceptionsTextExecute
    end
  end
end
